{-# LANGUAGE LambdaCase #-}
module Imp where

import Grammar
import Data.List (intercalate, intersperse)
import Prelude hiding (fail)
import qualified Data.Maybe as Maybe
import Control.Monad ((>=>), when, MonadFail ())
import Data.Data (typeOf)
import Data.Foldable (forM_, foldrM)

data CType = CIntT | CStructT [CType] | CFuncT CType [CType] | CVoidT | CPointerT CType | CTConstr String [CType] deriving (Show, Eq)

data CDecl = CFunc CType String [(CType, String)] String [CStmt] deriving Show

data CStmt = CAssn CType String CExpr String | CReturn CExpr String deriving Show

data CExpr = CInt Int | CIdent String | CApp CExpr [CExpr] | CProj CExpr CExpr | CStruct [CExpr] | CCast CType CExpr | CAddr CExpr | CDeref CExpr deriving Show

output :: [ANFFunc] -> SB ()
output fs = mapM outputFunc fs >>= \a -> (run.writeFile "test2.c".unlines . map toCString) a >> typeCheck [] a

outputType :: FBaseType -> SB CType
outputType t = case t of
    FTConstr s args -> case s of
        "Int" -> return CIntT
        "Tuple" -> mapM outputType args >>= \args2-> return $ CPointerT $ CStructT args2
        "Function" -> case args of
            [a,b] -> do
                a2 <- outputType a
                b2 <- outputType b
                return $ CFuncT b2 [CPointerT CVoidT, a2]
            _ -> undefined
        "_Void" -> return CVoidT
        "Void" -> return CVoidT
        "Pointer" -> CPointerT <$> outputType (head args)
        _ -> mapM outputType args >>= \args2-> return $ CTConstr s args2
    FForall i t -> fail "forall found when generating imperative code"
    FTVar i -> fail "type variable found when generating imperative code"

outputFunc :: ANFFunc -> SB CDecl
outputFunc (HoistedFunc id args entry js) = do
    t <- getType id
    let (FTConstr "Function" [_, b]) = t
    ts <- mapM getType args
    ts <- mapM outputType ts
    (ts, swaps) <- return $ foldr (\(t, i) (ts, swaps)->case t of
        CFuncT ret args -> (CIntT:ts, (t, i):swaps)
        _ -> (t:ts, swaps)) ([], []) (zip ts args)
    c <- Maybe.fromMaybe undefined <$> getIntent id
    b2 <- outputType b
    body <- outputEntry entry
    isMain <- (==id) <$> getMain
    return $ CFunc b2 (if isMain then "main" else 'g':show id) (if isMain then [] else zipWith (\a b->(b, 'l':show a)) args ts) c (foldr (\(t, i) b->CAssn t ('l':show i) (CCast t (CIdent $ 'l':show i)) ("unpacking closure l"++show i) : b) body swaps)

outputEntry :: ANFJoin -> SB [CStmt]
outputEntry (HoistedJoin _ _ body) = outputExpr body

outputExpr :: ANFTerm -> SB [CStmt]
outputExpr e = case e of
    ANFHalt val -> outputVal val >>= \(s,c)-> return [CReturn s c]
    ANFFunc id args body cont -> fail "func in straightline anf"
    ANFJoin id mbV body cont -> fail "join in straightline anf"
    ANFJump id mbV -> fail "jump not implemented yet"
    ANFApp id isGlobal f args cont -> do
        c <- Maybe.fromMaybe undefined <$> getIntent id
        ss <- mapM (outputVal >=> return.fst) args
        rest <- outputExpr cont
        t <- getType id
        let name = if isGlobal then 'g' else 'l'
        t2 <- outputType t
        return $ CAssn t2 ('l':show id) (CApp (CIdent $ name : show f) ss) c : rest
    ANFTuple id conjuncts cont -> do
        c <- Maybe.fromMaybe undefined <$> getIntent id
        cs <- mapM (outputVal >=> return.fst) conjuncts
        rest <- outputExpr cont
        t <- getType id
        t2 <- outputType t
        let castt = case t2 of
                CPointerT t -> t
                _ -> error ""
        return $ CAssn t2 ('l': show id) (CAddr $ CCast castt $ CStruct cs) c : rest
    ANFProj id tpl idx cont -> do
        c <- Maybe.fromMaybe undefined <$> getIntent id
        rest <- outputExpr cont
        t <- getType id
        t2 <- outputType t
        return $ CAssn t2 ('l':show id) (CProj (CDeref $ CIdent $ 'l':show tpl) (CInt idx)) c : rest
    ANFCast id t v cont -> do
        c <- Maybe.fromMaybe undefined <$> getIntent id
        rest <- outputExpr cont
        t2 <- outputType t
        (e, c2) <- outputVal v
        return $ CAssn t2 ('l':show id) (CCast t2 e) c : rest

outputVal :: ANFVal -> SB (CExpr, String)
outputVal v = case v of
    ANFVar id -> getIntent id >>= \c->return (CIdent $ "l"++show id, Maybe.fromMaybe undefined c)
    ANFInt i -> return (CInt i, "")
    ANFGlobal id -> getIntent id >>= \c->return (CIdent $ "g"++show id, Maybe.fromMaybe undefined c)

toCString :: CDecl -> String
toCString c = case c of
    CFunc retT name args comment body -> strTy retT ++ " " ++ name ++ "(" ++ intercalate ", " (map (uncurry strDecl) args) ++ ") {\n    // " ++ comment ++ "\n    " ++ intercalate "\n    " (map strStmt body) ++ "\n}"

strStmt :: CStmt -> [Char]
strStmt stmt = case stmt of
    CAssn t name expr comment -> strDecl t name ++ " = " ++ strExpr expr ++ "; // " ++ comment
    CReturn e comment -> "return " ++ strExpr e ++ "; // " ++ comment

strExpr :: CExpr -> [Char]
strExpr e = case e of
    CInt i -> show i
    CIdent name -> name
    CApp foo bars -> strExpr foo ++ "(" ++ intercalate ", " (map strExpr bars) ++ ")"
    CProj e (CInt i) -> strExpr e ++ ".p" ++ show i
    CProj e _ -> error ""
    CStruct es -> "{" ++ intercalate ", " (map strExpr es) ++ "}"
    CCast t e -> "("++strTy t++")"++strExpr e
    CAddr e -> "(&" ++ strExpr e ++ ")"
    CDeref e -> "(*" ++ strExpr e ++ ")"

strTy :: CType -> String
strTy ty = case ty of
    CIntT -> "int"
    CStructT ts -> "struct{" ++ concat (zipWith (\i t -> strDecl t ('p':show i) ++ ";") [0..] ts) ++ "}"
    CFuncT ret args -> strTy ret ++ "(*)(" ++ intercalate "," (map strTy args) ++ ")"
    CVoidT -> "void"
    CPointerT t -> strTy t ++ "*"
    CTConstr s ts -> s ++ if null ts then "" else "<" ++ intercalate ", " (map strTy ts) ++ ">"

strDecl :: CType -> String -> String
strDecl ty ident = case ty of
    CFuncT ret args -> strTy ret ++ " (*" ++ ident ++ ")(" ++ intercalate "," (map strTy args) ++ ")"
    CPointerT t -> strTy t ++ " *" ++ ident
    _ -> strTy ty ++ " " ++ ident

typeCheck :: [(String, CType)] -> [CDecl] -> SB ()
typeCheck scope lines = case lines of
    CFunc ty name args comment body : rest -> do
        scope <- return $ map (\(a,b)->(b,a)) args ++ (name, CFuncT ty (map fst args)) : scope
        foldrM (\stmt scope->case stmt of
            CAssn ty name e comment -> do
                t <- typeOf scope e
                if t == ty then
                    return $ (name, ty) : scope
                else
                    fail $ "variable " ++ name ++ " assigned to expr of wrong type (" ++ strTy ty ++ " != " ++ strTy t ++ " in `" ++ strStmt stmt ++ "`)"
            CReturn e comment -> do
                t <- typeOf scope e
                when (t /= ty) $ fail $ "body of " ++ name ++ " doesn't match return type (" ++ strTy t ++ " != " ++ strTy ty ++ " in " ++ strStmt stmt ++ ")"
                return scope
            ) scope $ reverse body
        typeCheck scope rest
    [] -> return ()
    where
        typeOf scope e = case e of
            CInt i -> return CIntT
            CIdent name -> case lookup name scope of
                Nothing -> fail $ "unknown identifier " ++ name ++ " in C codegen (" ++ strExpr e ++ ")"
                Just t -> return t
            CApp e2 es -> typeOf scope e2 >>= \case
                CFuncT ret args -> do
                    when (length args /= length es) $ fail $ "function " ++ strExpr e2 ++ " called with too few arguments in C codegen (" ++ strExpr e ++ ")"
                    forM_ (zip args es) $ \(t,e3)-> do
                        t2 <- typeOf scope e3
                        if t == t2 then
                            return()
                        else
                            fail $ "args are wrong type in C codegen ("++strTy t++" != "++strTy t2++" in " ++ strExpr e ++ ", arg `"++strExpr e3++": "++strTy t2++"`)"
                    return ret
                t -> fail $ "non-function called as function in C codegen ("++strTy t++")"
            CProj e2 (CInt i) -> typeOf scope e2 >>= \case
                CStructT ts ->
                    if length ts > i then
                        return (ts !! i)
                    else
                        fail $ "projection "++show i++" into struct that is out of bounds "++show (length ts)++" in C codegen (" ++ strExpr e ++ ")"
                t -> fail $ "projection into nonstruct "++strTy t++" in C codegen"
            CProj _ _ -> fail $ "projection with non-constant argument in C codegen (" ++ strExpr e ++ ")"
            CStruct es -> CStructT <$> mapM (typeOf scope) es
            CCast t e -> return t
            CAddr e -> CPointerT <$> typeOf scope e
            CDeref e -> typeOf scope e >>= \case
                CPointerT t -> return t
                _ -> fail $ "dereferencing nonpointer " ++ strExpr e