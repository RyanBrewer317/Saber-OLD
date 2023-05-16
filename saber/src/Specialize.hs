module Specialize where
import GHC.IOArray (IOArray, readIOArray, writeIOArray)
import Data.IORef (IORef, readIORef, newIORef)
import Grammar
import Typecheck (getFirstArgType)
import Control.Monad (zipWithM, when)
import Data.Foldable (foldrM, Foldable (toList))
import qualified Data.Map as Map
import Data.Map (fromList, union)
import Prelude hiding (fail)

specialize :: IOArray Int (IORef BaseType) -> [Stmt] -> SB [Stmt]
specialize types ast = do
    ts <- freezeTypes types ast
    setTypes ts
    specializeAST Map.empty ast

type ResEnv = Map.Map Int (Int, Int, Expr)

specializeAST :: ResEnv -> [Stmt] -> SB [Stmt]
specializeAST renv stmts = case stmts of
    stmt:rest -> case stmt of
        FuncDec id xid e -> do
            t <- getType id ? "getting type of funcdec " ++ show id
            isp <- isPolymorphic t
            if isp then do
                specializeAST (Map.insert id (id, xid, e) renv) rest
            else do
                s <- go renv e
                r <- specializeAST renv rest
                return $ FuncDec id xid s : r
    [] -> do
        return []

-- | Determine if there are type variables in the type, and fail on impossible cases
isPolymorphic :: FBaseType -> SB Bool
isPolymorphic tio = case tio of
    FForall vio tio2 -> return True
    FTVar id -> fail $ "found an unboud type variable with id " ++ show id
    FTConstr s [] -> return False
    FTConstr s ios -> and <$> mapM isPolymorphic ios

-- | produce a function with all instances of `id` and `xid` consistently updated to two fresh new variable IDs
funcDecToLam :: Int -> Int -> Expr -> SB (Int, Int, Expr)
funcDecToLam id xid e = do
    newId <- fresh $ "specialization of " ++ show id
    newXId <- fresh $ "specialization of " ++ show id ++ " (arg " ++ show xid ++ ")"
    return (newId, newXId, sub id newId $ sub xid newXId e)
    where
        sub old new e = case e of
            Var id isGlobal i -> if i == old then Var id isGlobal new else e
            IntLit _ -> e
            Lam id xid e2 -> Lam id xid $ sub old new e2
            App id f a -> App id (sub old new f) (sub old new a)
            Let id xid v e -> Let id xid (sub old new v) (sub old new e)

go :: ResEnv -> Expr -> SB Expr
go renv e = case e of
    Let id xid v e -> (getType xid >>= \xt-> isPolymorphic xt) ? "in `go` let binding " ++ show id >>= \isp->
        if isp then do
            error "todo" -- there shouldn't be polymorphism left, even from local let-binding
            -- TODO: this assumption is probably wrong, read Stefan Kaes on what to do here (yuck)
        else do -- convert this to (\x.e)(v) since let-polymorphism has already happened
            et <- getType id ? "in `go` let binding"
            xt <- getType xid
            fid <- fresh $ "lambda for converting let " ++ show id ++ " into an immediate application"
            setType fid $ FTConstr "Function" [xt, et]
            v2 <- go renv v
            e2 <- go renv e
            return $ App id (Lam fid xid e2) v2
    Var id isGlobal i -> case Map.lookup i renv of
        Nothing -> return e
        Just (fid, fxid, fe) -> do
            localT <- getType id ? "getting the local type of function " ++ show id
            let (FTConstr "Function" [xt, _]) = localT
            (newId, newXId, e) <- funcDecToLam fid fxid fe
            setType newId localT
            setType newXId xt
            return $ Lam newId newXId e
    IntLit {} -> return e
    Lam id xid e -> Lam id xid <$> go renv e
    App id f a -> do
        f2 <- go renv f
        a2 <- go renv a
        return $ App id f2 a2

freezeTypes :: IOArray Int (IORef BaseType) -> [Stmt] -> SB (Map.Map Int FBaseType)
freezeTypes types ast = case ast of
    stmt:stmts -> case stmt of
        FuncDec id xid e -> do
            t <- run $ readIOArray types id >>= freeze
            xt <- run $ readIOArray types xid >>= freeze
            m <- freezeTypesExpr types e
            m2 <- freezeTypes types stmts
            return $ Map.insert id t $ Map.insert xid xt $ union m m2
    [] -> return Map.empty

freezeTypesExpr :: IOArray Int (IORef BaseType) -> Expr -> SB (Map.Map Int FBaseType)
freezeTypesExpr types ast = case ast of
    Var id isGlobal i -> do
        t <- run $ readIOArray types id >>= freeze
        return $ fromList [(id, t)]
    IntLit i -> return Map.empty
    Lam id xid e -> do
        ft <- run $ readIOArray types id  >>= freeze
        xt <- run $ readIOArray types xid >>= freeze
        m  <- freezeTypesExpr types e
        return $ Map.insert id ft $ Map.insert xid xt m
    App id f a -> do
        t  <- run $ readIOArray types id >>= freeze
        fm <- freezeTypesExpr types f
        am <- freezeTypesExpr types a
        return $ Map.insert id t $ fm `union` am
    Let id xid v e -> fail "let found when specializing, after lets should be gone"

conversions :: FBaseType -> FBaseType -> [(Int, FBaseType)]
conversions t1 t2 = do
    case t1 of
        FTVar id -> [(id, t2)]
        FForall v t -> conversions t t2
        FTConstr s ts -> case t2 of
            FTConstr _ ts2 -> concat $ zipWithM conversions ts ts2
            FForall _ t -> conversions t1 t
            FTVar id -> [(id, t1)]
