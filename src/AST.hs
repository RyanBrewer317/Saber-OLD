{-# LANGUAGE OverloadedStrings #-}

module AST where
import Grammar
import Database.SQLite.Simple
import qualified Text.Parsec as P
import qualified Data.Text as Text
import Data.Foldable (foldrM)
import Prelude hiding (fail)
import Control.Monad (when)

buildSB :: [StmtSyntax] -> SB [Stmt]
buildSB stmts = do
    conn <- run $ open "saber.db"
    sql conn "CREATE TABLE IF NOT EXISTS locations (id INTEGER PRIMARY KEY, src TEXT, startline INTEGER, startcolumn INTEGER)"
    sql conn "CREATE TABLE IF NOT EXISTS intconstants (id INTEGER PRIMARY KEY, val INTEGER)"
    sql conn "CREATE TABLE IF NOT EXISTS realnames (id INTEGER PRIMARY KEY, name TEXT)"
    (_, out) <- foldrM (\stmt (renames, b)->buildStmt conn renames stmt >>= \(renames2, stmt2)-> return(renames2, stmt2:b)) ([], []) $ reverse stmts
    run $ close conn
    return $ reverse out

locquery :: Int -> P.SourcePos -> [Char]
locquery id pos = let (src, startline, startcol) = (P.sourceName pos, P.sourceLine pos, P.sourceColumn pos) in "INSERT INTO locations(id, src, startline, startcolumn) VALUES (" ++ show id ++ ", \"" ++ src ++ "\", " ++ show startline ++ ", " ++ show startcol ++ ")"
sqlpos :: Connection -> Int -> P.SourcePos -> SB ()
sqlpos conn id pos = sql conn $ locquery id pos

buildStmt :: Connection -> [(String, (Bool, Int))] -> StmtSyntax -> SB ([(String, (Bool, Int))], Stmt)
buildStmt conn renames stmt = case stmt of 
    FuncDecSyntax pos name arg body -> do
        id <- fresh name
        let q = locquery id pos
        when (name == "main") $ setMain id
        sql conn q
        sql conn $ "INSERT INTO realnames(id, name) VALUES (" ++ show id ++ ", \"" ++ name ++ "\")"
        argid <- fresh ""
        sql conn $ "INSERT INTO realnames(id, name) VALUES (" ++ show argid ++ ", \"" ++ arg ++ "\")"
        body2 <- buildExpr conn ((name, (True, id)) : (arg, (False, argid)) : renames) body
        return ((name, (True, id)) : renames, FuncDec id argid body2)

buildExpr :: Connection -> [(String, (Bool, Int))] -> ExprSyntax -> SB Expr
buildExpr conn renames expr = case expr of
    VarSyntax pos name -> case lookup name renames of
        Nothing -> fail $ "unknown identifier " ++ name
        Just (isGlobal, i) -> do
            id <- fresh $ "variable " ++ name ++ "(usage " ++ show pos ++ ")"
            sqlpos conn id pos
            return $ Var id isGlobal i
    IntLitSyntax pos i -> do
        id <- fresh $ "literal " ++ show i ++ " at " ++ show pos
        let q = "INSERT INTO intconstants(id, val) VALUES (" ++ show id ++ ", " ++ show i ++ ")"
        sql conn q
        sqlpos conn id pos
        return $ IntLit id
    LamSyntax pos x e -> do
        id <- fresh $ "lambda at " ++ show pos
        sqlpos conn id pos
        xid <- fresh ""
        sql conn $ "INSERT INTO realnames(id, name) VALUES (" ++ show xid ++ ", \"" ++ x ++ "\")"
        e2 <- buildExpr conn ((x, (False, xid)):renames) e
        return $ Lam id xid e2
    AppSyntax pos foo bar -> do
        id <- fresh $ "application at " ++ show pos
        sqlpos conn id pos
        foo2 <- buildExpr conn renames foo
        bar2 <- buildExpr conn renames bar
        return $ App id foo2 bar2
    LetSyntax pos x v e -> do
        id <- fresh $ "let-bind of " ++ x ++ " at " ++ show pos
        sqlpos conn id pos
        xid <- fresh $ "var " ++ x ++ " bound in let at " ++ show pos
        sql conn $ "INSERT INTO realnames(id, name) VALUES (" ++ show xid ++ ", \"" ++ x ++ "\")"
        v2 <- buildExpr conn renames v
        e2 <- buildExpr conn ((x, (False, xid)):renames) e
        return $ Let id xid v2 e2

