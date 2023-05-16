module Desugar where
import Grammar
import Gen
import Text.Parsec (SourcePos)
import Data.Foldable (foldrM)

locate :: SourcePos -> a -> Located a
locate pos a = Loc {val=a, start=pos}


alphaStmt :: Gen -> Located SyntaxStmt -> [(String, Int)] -> Either String (Gen, (Located Stmt, [(String, Int)]))
alphaStmt gen Loc{val=stmt,start=pos} renames = case stmt of
    SFuncDec name args body ->
        let (gen2, args2) = foldr (\arg (genx, argsx)->withFresh genx $ \genx2 i-> (genx2, (arg, i):argsx)) (gen, []) args in
        withFreshM gen2 $ \gen3 i-> do
            (gen4, body2) <- alpha gen3 body $ (name, i) : args2 ++ renames
            return (gen4, (Loc (FuncDec name i args2 body2) pos, (name, i):renames))

alphaStmts :: Gen -> [Located SyntaxStmt] -> Either String (Gen, ([Located Stmt], [(String, Int)]))
alphaStmts gen = foldrM (\stmt (genx, (stmts, renames))->alphaStmt genx stmt renames >>= \(genx2, (stmt2, renames2))->return (genx2, (stmt2:stmts, renames2++renames))) (gen, ([], []))

alpha :: Gen -> STerm -> [(String, Int)] -> Either String (Gen, Located Term)
alpha gen Loc {val=t, start=pos} renames = case t of
    SVar n -> case lookup n renames of
        Nothing -> err $ "unknown identifier " ++ n
        Just i -> right gen $ Var n i
    SInt n -> right gen $ IntLit n
    SLam x e ->
        withFreshM gen $ \gen2 i-> do
            (gen3, e2) <- alpha gen2 e $ (x, i):renames
            right gen3 $ Lam x i e2
    SApp t1 t2 -> do
        (gen2, t12) <- alpha gen t1 renames
        (gen3, t22) <- alpha gen2 t2 renames
        right gen3 $ App t12 t22
    SLet x v e -> do
        (gen2, v2) <- alpha gen v renames
        withFreshM gen2 $ \gen3 i-> do
            (gen4, e2) <- alpha gen3 e $ (x, i):renames
            right gen4 $ Let x i v2 e2
    where
        err s = Left $ show pos ++ ": " ++ s
        right gen v = Right (gen, locate pos v)
