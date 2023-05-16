module MyLLVM where

import LLVM.AST.Name ( mkName, Name(Name) )
import Grammar
import LLVM.AST.Type (Type(..))
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
import Data.Foldable ( foldlM, foldrM )
import LLVM.AST.Instruction ( Instruction(PtrToInt, IntToPtr, Call, GetElementPtr, Store), Named((:=)), Terminator(Br, Ret) )
import LLVM.AST.CallingConvention (CallingConvention(C))
import LLVM.AST.Global (Global(parameters, name, returnType, basicBlocks, linkage))
import LLVM.AST.Linkage (Linkage(External))
import Database.SQLite.Simple hiding ((:=))
import qualified Data.Text as Text
import qualified LLVM.AST.Instruction as I
import Prelude hiding (fail)
import Control.Monad (filterM, (>=>), when, unless)

intType :: Type
intType = IntegerType 64
ptrType :: Type -> Type
ptrType t = PointerType t (AddrSpace 0)

fbtToType :: FBaseType -> SB Type
fbtToType fbt = case fbt of
    FForall i fbt2 -> dumpTypes >> fail ("polymorphic type found; tvar="++show i)
    FTVar i -> dumpTypes >> fail ("type variable found; tvar="++show i)
    FTConstr s fbts -> mapM fbtToType fbts >>= \ts-> case s of
        "Function" -> let [a,b] = ts in return $ ptrType $ FunctionType b [a] False
        "Int" -> return intType
        "Tuple" -> mapM fbtToType fbts >>= \ts-> return $ ptrType $ StructureType False ts
        "_Void" -> return VoidType
        "" -> fail "indexing unassigned type id"
        _ -> fail $ "unknown type constructor " ++ s

preprocess :: [ANFJoin] -> SB (Map.Map Int Int)
preprocess = foldlM go Map.empty
    where
        go spills join = case join of
            HoistedJoin j (Just p) _ -> fresh ("spill slot for join point "++show j) >>= \i->return $ Map.insert j i spills
            HoistedJoin _ Nothing _ -> return spills

newtype RealName = RealName String
instance FromRow RealName where
    fromRow = RealName <$> field

getName :: Connection -> Int -> SB (Maybe String)
getName conn id = r <$> run (query_ conn $ Query $ Text.pack $ "SELECT name FROM realnames WHERE id = " ++ show id :: IO [RealName])
    where
        r [] = Nothing
        r (RealName n:_) = Just n

lowerVal :: Connection -> Type -> ANFVal -> SB Operand
lowerVal conn t v = case v of
    ANFInt i -> return $ ConstantOperand $ Int 64 (fromIntegral i)
    ANFVar i -> return $ LocalReference t $ mkName $ "x"++show i
    ANFGlobal i -> getMain >>= (\b->return $ ConstantOperand $ GlobalReference t $ mkName $ if b then "main" else "x"++show i) . (i==)

typeOf :: ANFVal -> SB Type
typeOf v = case v of
    ANFInt i -> return intType
    ANFVar id -> (fbtToType =<< getType id) ? "getting LLVM type of variable " ++ show id
    ANFGlobal id -> (fbtToType =<< getType id) ? "getting LLVM type of global variable " ++ show id

lowerTerm :: Connection -> Map.Map Int Int -> ANFTerm -> SB (Terminator, [Named Instruction])
lowerTerm conn spills e = case e of
    ANFHalt v -> typeOf v >>= \vt->lowerVal conn vt v >>= \v->return (Ret (Just v) [], [])
    ANFFunc {} -> fail "func found in straightline ANF"
    ANFJoin {} -> fail "join found in straightline ANF"
    ANFJump j (Just v) -> case Map.lookup j spills of
        Nothing -> fail "jump found with no spill"
        Just slot -> do
            let slotv = ANFVar slot
            slott <- typeOf slotv ? "getting type of jump slot " ++ show slot
            slot <- lowerVal conn slott slotv ? "lowering jump slot " ++ show slot
            v <- let x = ANFVar v in typeOf x >>= \xt->lowerVal conn xt x ? "lowering jump value " ++ show v
            return (Br (mkName $ "x"++show j) [], [Do $ Store False slot v Nothing 64 []])
    ANFJump j Nothing -> return (Br (mkName $ "x"++show j) [], [])
    ANFApp id isGlobal fid argids cont -> do
        args <- mapM (\a-> typeOf a>>= \at->(lowerVal conn at a ? "lowering arg")>>= \a->return (a, [])) argids
        -- cast_i <- fresh $ "cast id for casting " ++ show fid ++ " to the appropriate function type"
        outt <- (fbtToType =<< getType id) ? "getting LLVM type of application " ++ show id
        argtypes <- tail <$> mapM typeOf argids
        let ft = FunctionType outt (intType:argtypes) False
        -- let cast = mkName ("x"++show cast_i) := IntToPtr (LocalReference intType $ mkName $ "x"++show fid) ft []
        -- f <- lowerVal conn ft (ANFGlobal cast_i) ? "lowering cast"
        (t, is) <- lowerTerm conn spills cont ? "lowering continuation"
        -- return (t, cast:(mkName ("x"++show id) := Call Nothing C [] (Right f) args [] []):is)
        f <- lowerVal conn ft (ANFGlobal fid) ? "lowering function"
        return (t, (mkName ("x"++show id) := Call Nothing C [] (Right f) args [] []):is)
    ANFTuple id xs cont -> do
        ptr <- fresh $ "id for the variable holding memory returned by malloc for tuple " ++ show id
        let size = length xs * 8
        sizev <- lowerVal conn undefined (ANFInt size)
        let alloc = mkName ("x"++show ptr) := Call Nothing C [] (Right $ ConstantOperand $ GlobalReference (FunctionType (ptrType intType) [intType] False) $ mkName "malloc") [(sizev, [])] [] []
        let populate idx x = do
                i <- fresh $ "id for pointer into tuple " ++ show id ++ " slot " ++ show idx
                idxv <- lowerVal conn undefined $ ANFInt idx
                t <- typeOf $ ANFVar id
                let gep = mkName ("x"++show i) := GetElementPtr False (LocalReference (ptrType t) $ mkName $ "x"++show ptr) [idxv] []
                let store v = Do $ Store False (LocalReference (ptrType intType) $ mkName $ "x"++show i) v Nothing 64 []
                case x of
                    ConstantOperand (GlobalReference _ g) -> do
                            i2 <- fresh $ "id for cast from pointer to int (tuple " ++ show id ++ ", index " ++ show idx ++ ")"
                            let cast = mkName ("x"++show i2) := I.PtrToInt x intType []
                            i2v <- lowerVal conn intType $ ANFVar i2
                            return [gep, cast, store i2v]
                    LocalReference _ n -> return [gep, store x]
                    _ -> fail $ "unknown operand during LLVM codegen of tuple " ++ show id ++ ": " ++ show x
        stores <- foldrM (\(i, x) b->typeOf x>>= \xt->lowerVal conn xt x >>= (fmap (: b) . populate i)) [] (zip [0..] xs)
        (t, is) <- lowerTerm conn spills cont
        return (t, alloc:(mkName ("x"++show id) := I.PtrToInt (LocalReference (ptrType intType) $ mkName $ "x"++show ptr) intType []):concat stores++is)
    ANFProj id tpl idx cont -> do
        addr <- fresh $ "id for holding calculated pointer tuple-"++show tpl++"-index-"++show idx
        tuple <- fresh $ "id for holding cast from int-var " ++ show tpl++" into a pointer"
        (t, is) <- lowerTerm conn spills cont
        tplt <- typeOf $ ANFVar tpl
        tplv <- lowerVal conn tplt $ ANFVar tpl
        idxv <- lowerVal conn undefined $ ANFInt idx
        return (t,
            (mkName ("x"++show tuple) := IntToPtr tplv (ptrType intType) []):
            (mkName ("x"++show addr)  := GetElementPtr False (LocalReference (ptrType intType) $ mkName $ "x"++show tuple) [idxv] []):
            (mkName ("x"++show id)    := Load False (LocalReference (ptrType intType) $ mkName $ "x"++show addr) Nothing 64 []):
            is)

lowerBlock :: Connection -> Map.Map Int Int -> ANFJoin -> SB (Terminator, [Named Instruction])
lowerBlock conn spills (HoistedJoin j mbVal e) = case mbVal of
    Just v -> case Map.lookup j spills of
        Just slot -> do
            (terminator, rest) <- lowerTerm conn spills e
            slotv <- lowerVal conn intType $ ANFVar slot
            return (terminator, (mkName ("x"++show v) := Load False slotv Nothing 64 []):rest)
        Nothing -> fail "join without spill found"
    Nothing -> lowerTerm conn spills e

lowerFunc :: Connection -> Map.Map Int Int -> ANFFunc -> SB Definition
lowerFunc conn spills (HoistedFunc f args body joins) = do
    (terminator, block) <- lowerBlock conn spills body
    isMain <- (f==) <$> getMain
    args <- return $ if isMain then [] else args
    (block, terminator) <-
        if isMain then
            case terminator of
                Ret (Just o) x0 -> do
                    zerov <- lowerVal conn undefined $ ANFInt 0
                    return (block++[Do $ Call Nothing C [] (Right $ ConstantOperand $ GlobalReference (FunctionType VoidType [intType] False) $ mkName "printInt") [(o, [])] [] []], Ret (Just zerov) [])
                _ -> return (block, terminator)
        else
            return (block, terminator)
    (args, block) <- if isMain then return (args, block) else do
        let closureArgOld = head args
        closureArgType <- typeOf (ANFVar closureArgOld)
        closureArgId <- fresh $ "id for arg " ++ show closureArgOld ++ " as int; function: " ++ show f
        setType closureArgId $ FTConstr "Int" []
        args <- return $ closureArgId : tail args
        closureArg <- lowerVal conn intType (ANFVar closureArgId)
        block <- return $ (mkName ("x"++show closureArgOld) := IntToPtr closureArg closureArgType []):block
        return (args, block)
    argts <- mapM (typeOf . ANFVar) args
    t <- fbtToType =<< getType f
    case t of
        PointerType (FunctionType outt _ _) _ ->
            return $ GlobalDefinition functionDefaults
                { name = mkName $ if isMain then "main" else "x" ++ show f
                , parameters = (zipWith (\i t-> Parameter t (mkName $ "x" ++ show i) []) args argts, False)
                , returnType = outt
                , basicBlocks = [BasicBlock (mkName "entry") block (Do terminator)]
                }
        _ -> do
            dumpTypes
            fail $ "not a pointer to a function: " ++ show f

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

mkModule :: Connection -> Map.Map Int Int -> [ANFFunc] -> SB Module
mkModule conn spills fs = do
    fs2 <- foldrM (\a b->(:b)<$>lowerFunc conn spills a) [] fs
    let bs = map emitBuiltin builtins
    return $ defaultModule { moduleName = let (Name s) = mkName "saber" in s, moduleDefinitions = bs++fs2}

output :: [ANFFunc] -> SB Module
output fs = do
    conn <- run $ open "saber.db"
    mkModule conn Map.empty fs <* run (close conn)

