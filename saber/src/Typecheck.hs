{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
module Typecheck where
import Grammar
import Data.Foldable (foldrM)
import qualified Data.Text as Text
import Prelude hiding (fail)
import Data.IORef (newIORef, IORef, readIORef, writeIORef)
import Data.List (union, intercalate)
import GHC.IOArray (IOArray, newIOArray, writeIOArray, readIOArray)
import Database.SQLite.Simple (query_, open, Query (Query), FromRow (fromRow), field, close)

assignBasicTypes :: [Stmt] -> SB (IOArray Int (IORef BaseType))
assignBasicTypes ast = do
    dummy <- run $ newIORef $ TConstr "" []
    types <- run $ newIOArray (0, 1000000) dummy
    basicStmts types ast
    return types

basicStmts :: IOArray Int (IORef BaseType) -> [Stmt] -> SB ()
basicStmts types ast =
    case ast of
        [] -> return ()
        stmt:stmts -> basicStmt types stmt >> basicStmts types stmts

newtype RealName = RealName String deriving Eq
instance FromRow RealName where
    fromRow = RealName <$> field

basicStmt :: IOArray Int (IORef BaseType) -> Stmt -> SB ()
basicStmt types stmt = case stmt of
    FuncDec id argid e -> do
        argti <- fresh $ "type variable id for argument " ++ show argid ++ " of top-level function " ++ show id
        isMain <- (id==) <$> getMain
        argt <- run $ newIORef $ if isMain then TConstr "_Void" [] else TVar argti
        run $ writeIOArray types argid argt
        basicExpr types e
        et <- run $ readIOArray types $ getId e
        t <- run $ newIORef $ TConstr "Function" [argt, et]
        t <- run $ generalize t
        argt <- run $ getFirstArgType t
        run $ writeIOArray types argid argt
        run $ writeIOArray types id t

getFirstArgType :: IORef BaseType -> IO (IORef BaseType)
getFirstArgType io = readIORef io >>= \case
    Forall vio tio -> getFirstArgType tio
    TConstr "Function" [a, _] -> return a
    TConstr _ _ -> error ""
    TVar _ -> error ""

basicExpr :: IOArray Int (IORef BaseType) -> Expr -> SB ()
basicExpr types expr = case expr of
    Var id isGlobal i -> do
        t <- run $ readIOArray types i
        t <- run $ instantiate [] t
        run $ writeIOArray types id t
    IntLit id -> run (newIORef (TConstr "Int" [])) >>= \t->run $ writeIOArray types id t
    Lam id xid e -> do
        xtype <- fresh $ "type variable id for the type of argument " ++ show xid ++ " in lambda " ++ show id
        xt <- run $ newIORef $ TVar xtype
        run $ writeIOArray types xid xt
        basicExpr types e
        etype <- run $ readIOArray types $ getId e
        t <- run $ newIORef $ TConstr "Function" [xt, etype]
        run $ writeIOArray types id t
    App id foo bar -> do
        basicExpr types foo
        fooT <- run $ readIOArray types (getId foo)
        basicExpr types bar
        barT <- run $ readIOArray types (getId bar)
        ti <- fresh $ "type variable id for the type of app "++show id
        t <- run $ newIORef $ TVar ti
        fooT2 <- run $ newIORef $ TConstr "Function" [barT, t]
        run $ writeIOArray types id t
        unify fooT fooT2
    Let id xid v e -> do
        basicExpr types v
        vt <- run $ readIOArray types $ getId v
        xt <- run $ generalize vt
        run $ writeIOArray types xid xt
        basicExpr types e
        et <- run $ readIOArray types $ getId e
        run $ writeIOArray types id et

showtype :: IORef BaseType -> IO String
showtype io = readIORef io >>= \case
    Forall vio tio -> readIORef vio >>= \case
        TVar i -> showtype tio >>= \s-> return $ "forall x" ++ show i ++ ". " ++ s
        _ -> showtype tio
    TVar i -> return $ "x" ++ show i
    TConstr s ios -> mapM showtype ios >>= \ss-> return $ s ++ " " ++ unwords ss

instantiate :: [(Int, IORef BaseType)] -> IORef BaseType -> IO (IORef BaseType)
instantiate renames io = readIORef io >>= \case
    Forall vio tio -> readIORef vio >>= \case
        TVar id -> do
            vio2 <- newIORef (TVar id)
            tio2 <- instantiate ((id, vio2):renames) tio
            newIORef $ Forall vio2 tio2
        _ -> instantiate renames tio
    TVar id -> case lookup id renames of
        Nothing -> return io
        Just vio -> return vio
    TConstr s ios -> mapM (instantiate renames) ios >>= \ios2->newIORef (TConstr s ios2)

generalize :: IORef BaseType -> IO (IORef BaseType)
generalize io = do
    fvs <- freevars [] io
    foldrM (\a b->newIORef $ Forall a b) io fvs
    where freevars skips io = do
            t <- readIORef io
            case t of
                TVar id -> return [io | id `notElem` skips]
                Forall io u -> readIORef io >>= \case
                    TVar id -> freevars (id:skips) u
                    _ -> freevars skips u
                TConstr s ts -> mapM (freevars skips) ts >>= \ls->return $ foldr union [] ls

unify :: IORef BaseType -> IORef BaseType -> SB ()
unify t1io t2io = do
    t1 <- run $ readIORef t1io
    t2 <- run $ readIORef t2io
    case t1 of
        TVar id -> do
            -- debug "unifying with tvar on left"
            out <- run $ writeIORef t1io t2
            run $ strbasetype t1io
            return out
        TConstr s ios -> case t2 of
            Forall vio tio -> unify t1io tio
            TVar id -> do
                -- debug "unifying with tvar on right"
                out <- run $ writeIORef t2io t1
                run $ strbasetype t2io
                return out
            TConstr s2 ios2 ->
                if s == s2 && length ios == length ios2 then
                    mapM_ (uncurry unify) $ zip ios ios2
                else fail $ "failed to unify " ++ s ++ " and " ++ s2
        Forall vio tio -> unify tio t2io
