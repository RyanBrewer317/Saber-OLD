module MyLLVM where

import LLVM.AST.Name ( mkName, Name(Name) )
import Grammar
import LLVM.AST.Type (i64, Type(..))
import LLVM.AST
    ( Operand(..),
      Instruction(Load),
      Definition(GlobalDefinition),
      functionDefaults,
      Parameter(Parameter),
      BasicBlock(BasicBlock),
      defaultModule,
      Module(moduleDefinitions, moduleName),
      Named(..) )
import LLVM.AST.Constant (Constant(GlobalReference, Int, PtrToInt))
import LLVM.AST.AddrSpace ( AddrSpace(AddrSpace) )
import qualified Data.Map as Map
import Data.Foldable ( Foldable(foldl') )
import Gen (withFresh, Gen, withFreshM)
import LLVM.AST.Instruction ( Instruction(PtrToInt, IntToPtr, Call, GetElementPtr, Store), Named((:=)), Terminator(Br, Ret) )
import LLVM.AST.CallingConvention (CallingConvention(Fast))
import LLVM.IRBuilder (call, runIRBuilder, emptyIRBuilder, extern)
import LLVM.AST.Global (Global(parameters, name, returnType, basicBlocks, linkage))
import LLVM.Pretty (ppllvm)
import Data.Text.Lazy as Lazy (unpack)
import Data.Map (union)
import LLVM.AST.Linkage (Linkage(External))
import Debug.Trace (trace)

intType :: Type
intType = IntegerType 64
ptrType :: Type -> Type
ptrType t = PointerType t (AddrSpace 0)
funcType = FunctionType intType [intType, intType] False

preprocess :: Gen -> [ANFJoin] -> (Gen, Map.Map Int Int)
preprocess g = foldl' go (g, Map.empty)
    where
        go (gen, spills) join = case join of
            HoistedJoin j (Just p) _ -> withFresh gen $ \gen2 i-> (gen2, Map.insert j i spills)
            HoistedJoin _ Nothing _ -> (gen, spills)

lowerVal :: ANFVal -> Operand
lowerVal v = case v of
    ANFInt i -> ConstantOperand $ Int 64 (fromIntegral i)
    ANFVar i -> LocalReference intType (mkName $ "x"++show i)
    ANFGlob i -> ConstantOperand $ GlobalReference (ptrType funcType) (mkName $ "x"++show i)

lowerTerm :: Gen -> Map.Map Int Int -> ANFTerm -> (Gen, (Terminator, [Named Instruction]))
lowerTerm gen spills e = case e of
    ANFHalt v -> (gen, (Ret (Just $ lowerVal v) [], []))
    ANFFunc {} -> error "func found in straightline ANF"
    ANFJoin {} -> error "join found in straightline ANF"
    ANFJump j (Just v) -> case Map.lookup j spills of
        Nothing -> error "jump found with no spill"
        Just slot -> (gen, (Br (mkName $ "x"++show j) [], [Do $ Store False (lowerVal $ ANFVar slot) (lowerVal $ ANFVar v) Nothing 64 []]))
    ANFJump j Nothing -> (gen, (Br (mkName $ "x"++show j) [], []))
    ANFApp i f args cont ->
        let args2 = map (\a->(lowerVal a, [])) args in
        withFresh gen $ \gen2 cast_i->
            let cast = mkName ("x"++show cast_i) := IntToPtr (LocalReference intType $ mkName $ "x"++show f) (ptrType funcType) [] in
            let (gen3, (terminator, rest)) = lowerTerm gen2 spills cont in
            (gen3, (terminator, cast:(mkName ("x"++show i) := Call Nothing Fast [] (Right $ LocalReference funcType $ mkName $ "x"++show cast_i) args2 [] []):rest))
    ANFTuple i xs cont ->
        withFresh gen $ \gen2 ptr->
            let size = length xs * 8 in
            let alloc = mkName ("x"++show ptr) := Call Nothing Fast [] (Right $ ConstantOperand $ GlobalReference (FunctionType (ptrType intType) [intType] False) $ mkName "malloc") [(lowerVal $ ANFInt size, [])] [] [] in
            let populate g index x =
                    withFresh g $ \g2 i->
                        let gep = mkName ("x"++show i) := GetElementPtr False (LocalReference (ptrType intType) $ mkName $ "x"++show ptr) [lowerVal $ ANFInt index] [] in
                        let store v = Do $ Store False (LocalReference (ptrType intType) $ mkName $ "x"++show i) v Nothing 64 [] in
                        case x of
                            ConstantOperand (GlobalReference _ g) ->
                                withFresh g2 $ \g3 i2->
                                    let cast = mkName ("x"++show i2) := LLVM.AST.Instruction.PtrToInt x intType [] in
                                    (g3, [gep, cast, store (lowerVal $ ANFVar i2)])
                            LocalReference _ n -> (g2, [gep, store x])
                            _ -> error ""
                in
            let (gen3, stores) = foldr (\(i, x) (genx, b)->let (genx2, isns) = populate genx i $ lowerVal x in (genx2, isns:b)) (gen2, []) (zip [0..] xs) in
            let (gen4, (terminator, rest)) = lowerTerm gen3 spills cont in
            (gen4, (terminator, alloc:(mkName ("x"++show i) := LLVM.AST.Instruction.PtrToInt (LocalReference (ptrType intType) $ mkName $ "x"++show ptr) intType []):concat stores++rest))
    ANFProj i tpl idx cont ->
        withFresh gen $ \gen2 addr->
            withFresh gen2 $ \gen3 tuple->
                let (gen4, (terminator, rest)) = lowerTerm gen3 spills cont in
                (gen4, (terminator,
                (mkName ("x"++show tuple) := IntToPtr (lowerVal $ ANFVar tpl) (ptrType intType) []):
                (mkName ("x"++show addr)  := GetElementPtr False (LocalReference (ptrType intType) $ mkName $ "x"++show tuple) [lowerVal $ ANFInt idx] []):
                (mkName ("x"++show i)     := Load False (LocalReference (ptrType intType) $ mkName $ "x"++show addr) Nothing 64 []):
                rest))

lowerBlock :: Gen -> Map.Map Int Int -> ANFJoin -> (Gen, (Terminator, [Named Instruction]))
lowerBlock gen spills (HoistedJoin j mbVal e) = case mbVal of
    Just v -> case Map.lookup j spills of
        Just slot ->
            let (gen2, (terminator, rest)) = lowerTerm gen spills e in
            (gen2, (terminator, (mkName ("x"++show v) := Load False (lowerVal $ ANFVar slot) Nothing 64 []):rest))
        Nothing -> error "join w no spill lol"
    Nothing -> lowerTerm gen spills e

lowerFunc :: Bool -> Gen -> Map.Map Int Int -> ANFFunc -> (Gen, Definition)
lowerFunc isMain gen spills (HoistedFunc f args body joins) =
    let (gen2, (terminator, block)) = lowerBlock gen spills body in
    let (block2, terminator2) = if isMain then case terminator of
          Ret (Just o) x0 -> (block++[Do $ Call Nothing Fast [] (Right $ ConstantOperand $ GlobalReference (FunctionType VoidType [intType] False) $ mkName "printInt") [(o, [])] [] []], Ret (Just $ lowerVal $ ANFInt 0) [])
          _ -> (block, terminator)
        else (block, terminator) in
    (gen2, GlobalDefinition functionDefaults
    { name = mkName $ if isMain then "main" else "x" ++ show f
    , parameters = (map (\i->Parameter intType (mkName $ "x"++show i) []) args, False)
    , returnType = intType
    , basicBlocks = [BasicBlock (mkName "entry") block2 (Do terminator2)]
    })



builtins :: [(String, [Type], Type)]
builtins = [ ("malloc", [intType], ptrType intType)
           , ("printInt", [intType], VoidType)
           ]

emitBuiltin :: (String, [Type], Type) -> Definition
emitBuiltin (name, argTypes, retType) = GlobalDefinition $ functionDefaults
            { name = mkName name
            , linkage = External
            , parameters = ([Parameter ty (mkName "") [] | ty <- argTypes], False)
            , returnType = retType
            }

mkModule :: Gen -> Map.Map Int Int -> [ANFFunc] -> [ANFJoin] -> (Gen, Module)
mkModule gen spills fs js =
    let (gen2, fs2) = foldr (\a (genx, b)->let (genx2, f) = lowerFunc False genx spills a in (genx2, f:b)) (gen, []) (init fs) in
    let (gen3, main) = lowerFunc True gen2 spills (last fs) in
    let (gen4, js2) = foldr (\a b->b) (gen3, []) js in
    let bs = map emitBuiltin builtins in
    (gen4, defaultModule { moduleName = let (Name s) = mkName "saber" in s, moduleDefinitions = bs++fs2++js2++[main]})

output :: Gen -> [ANFFunc] -> [ANFJoin] -> (Gen, Module)
output gen fs js =
    let (gen2, spills) = preprocess gen js in
    let (gen3, spills2) = foldr (\(HoistedFunc a b j js) (genx, s)->let (genx2, js2) = preprocess genx (j:js) in (genx2, js2 `union` s)) (gen2, spills) fs in
    mkModule gen2 spills2 fs js
