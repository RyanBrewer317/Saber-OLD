module Desugar where
import Grammar
import Gen
import Text.Parsec (SourcePos)

locate :: SourcePos -> a -> Located a
locate pos a = Loc {val=a, start=pos}

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
