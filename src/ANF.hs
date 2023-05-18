module ANF where
import Grammar
import Database.SQLite.Simple
    ( close, open, query_, field, FromRow(..), Query(Query) )
import qualified Data.Text as Text (pack)
import Data.List (union)
import Data.Foldable (foldl', foldrM, forM_)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Prelude hiding (fail)
import qualified Data.Maybe as Maybe

anf :: [Stmt] -> SB [ANFFunc]
anf ast = uncurry ($) <$> go ast
    where
        go :: [Stmt] -> SB (DiffList ANFFunc, [ANFFunc])
        go ast = case ast of
            stmt:stmts -> case stmt of
                FuncDec id argid e -> do
                    entry <- fresh $ "entrypoint join point for function " ++ show id
                    a <- convert e
                    a <- closureConvert Map.empty a
                    (fs, js, a) <- hoist a
                    (fs2, as) <- go stmts
                    dumpTypes
                    return (fs . fs2, HoistedFunc id [argid] (HoistedJoin entry Nothing a) (js []):as)
            [] -> return (id, [])

newtype Intconst = Intconst Int
instance FromRow Intconst where
    fromRow = Intconst <$> field

convert :: Expr -> SB ANFTerm
convert e = do
    conn <- run $ open "saber.db"
    go conn e (return . ANFHalt) <* run (close conn)
    where
        go conn e k = case e of
            Var id isGlobal i -> k $ if isGlobal then ANFGlobal i else ANFVar i
            IntLit id -> do
                (Intconst i) <- head <$> run (query_ conn $ Query $ Text.pack $ "SELECT val FROM intconstants WHERE id = " ++ show id :: IO [Intconst])
                k (ANFInt i)
            Lam id xid e -> do
                cont <- k (ANFVar id)
                e2 <- go conn e $ \t->return(ANFHalt t)
                return (ANFFunc id [xid] e2 cont)
            App id f a -> do
                cont <- k (ANFVar id)
                go conn f $ \f2->
                    go conn a $ \a2->
                        case f2 of
                            ANFVar id3 -> return (ANFApp id False id3 [a2] cont)
                            ANFGlobal id3 -> return (ANFApp id True id3 [a2] cont)
                            _ -> fail $ "calling int during ANF conversion: "++show f2
            Let id xid v e -> fail "let found during ANF conversion"

fvs :: ANFTerm -> [Int] -> [Int]
fvs t captures = case t of
    ANFHalt (ANFVar i) -> [i | i `notElem` captures]
    ANFHalt _ -> []
    ANFFunc f args body cont -> fvs body (f:args) `union` fvs cont [f]
    ANFJoin n m_n a b -> fvs a [] `union` fvs b []
    ANFJump i m_n -> []
    ANFApp i isGlobal f args cont -> union (fvs cont [i]) $ ([f | f `notElem` captures]) ++ foldr (\v b->case v of ANFVar i->if i `elem` captures then b else i:b ; _ -> b) [] args
    ANFTuple i conjuncts cont -> union (fvs cont [i]) $ foldr (\v b->case v of ANFVar i->if i `elem` captures then b else i:b ; _ -> b) [] conjuncts
    ANFProj i tpl idx cont -> [tpl | tpl `notElem` captures] `union` fvs cont [i]
    ANFCast i t (ANFVar x) cont -> [x | x `notElem` captures] `union` fvs cont [i]
    ANFCast i _ _ cont -> fvs cont [i]

putAtEnd :: (ANFVal -> ANFTerm) -> ANFTerm -> ANFTerm
putAtEnd f e = case e of
    ANFHalt v -> f v
    ANFFunc f2 args body cont -> ANFFunc f2 args body $ putAtEnd f cont
    ANFJoin i mbV body cont -> ANFJoin i mbV body $ putAtEnd f cont
    ANFJump i mbV -> e
    ANFApp i isGlobal f2 args cont -> ANFApp i isGlobal f2 args $ putAtEnd f cont
    ANFTuple i conjuncts cont -> ANFTuple i conjuncts $ putAtEnd f cont
    ANFProj i tpl idx cont -> ANFProj i tpl idx $ putAtEnd f cont
    ANFCast i t v cont -> ANFCast i t v $ putAtEnd f cont

closureConvert :: Map.Map Int FBaseType -> ANFTerm -> SB ANFTerm
closureConvert retCasts t = case t of
            ANFHalt v -> return t
            ANFFunc id argids body cont -> do
                env <- fresh $ "closure tuple for function " ++ show id
                setType env $ FTConstr "Int" []
                let free = filter (`notElem` argids) $ fvs body []
                freets <- mapM getType free
                t <- getType id
                let (FTConstr "Function" [argt, rett]) = t
                swappedArg <- case argt of
                    FForall _ _ -> undefined
                    FTVar _ -> undefined
                    FTConstr "Function" _ -> setType (head argids) (FTConstr "Int" []) >> return True
                    FTConstr _ _ -> return False
                swappedRet <- case rett of
                    FForall _ _ -> undefined
                    FTVar _ -> undefined
                    FTConstr "Function" _ -> return True
                    FTConstr _ _ -> return False
                t <- return $ FTConstr "Function" [if swappedArg then FTConstr "Int" [] else argt, if swappedRet then FTConstr "Int" [] else rett]
                setType id t
                body2 <- closureConvert (if swappedRet then Map.insert id rett retCasts else retCasts) body
                let body3 = fst (foldl' (\(e, i) x->(ANFProj x env i e, i+1)) (body2, 1) free)
                let body4 = if swappedArg then ANFCast (head argids) argt (ANFVar (head argids)) body3 else body3
                retVar <- fresh "casting the return value to the right type (closure conversion)"
                let body5 = if swappedRet then putAtEnd (\v->ANFCast retVar rett v $ ANFHalt $ ANFVar retVar) body4 else body4
                c <- getIntent id
                i2 <- fresh $ Maybe.fromMaybe "" c
                setType i2 t
                setType id $ FTConstr "Tuple" $ t : freets
                cont2 <- closureConvert (if swappedRet then Map.insert id rett retCasts else retCasts) cont
                let cont3 = let vs = map ANFVar free in ANFTuple id (ANFGlobal i2:vs) cont2
                return (ANFFunc i2 (env:argids) body3 cont3)
            ANFJoin id mbVal body cont -> do
                body2 <- closureConvert retCasts body
                cont2 <- closureConvert retCasts cont
                return (ANFJoin id mbVal body2 cont2)
            ANFJump id mbVal -> return t
            ANFApp id isGlobal fid argids cont ->
                if isGlobal then
                    closureConvert retCasts cont >>= \k-> return (ANFApp id isGlobal fid argids k)
                else do
                    ptr <- fresh $ "projection into closure tuple (application " ++ show id ++ ", tuple "++show fid++")"
                    closureT <- getType fid
                    debug $ show closureT
                    let ft = case closureT of
                            FTConstr "Tuple" (x:_) -> x
                            FTConstr "Function" _ -> closureT
                            _ -> error ""
                    let (FTConstr "Function" [argt, rett]) = ft
                    setType ptr ft
                    cont2 <- closureConvert retCasts cont
                    actualArgT <-case head argids of
                        ANFVar id -> getType id
                        ANFInt n -> return $ FTConstr "Int" []
                        ANFGlobal id -> getType id
                    let castArg = case (argt, actualArgT) of
                            (FTConstr "Int" [], FTConstr "Tuple" _) -> True
                            _ -> False
                    closureCast <- fresh "casting the function's closure to an int"
                    argCast <- fresh "casting the argument closure to an int"
                    setType closureCast $ FTConstr "Int" []
                    setType argCast $ FTConstr "Int" []
                    castretid <- fresh "return value before cast closure"
                    setType castretid $ FTConstr "Int" []
                    let castingRet = Maybe.isJust $ Map.lookup fid retCasts
                    let cont3 = case Map.lookup fid retCasts of
                          Nothing -> cont2
                          Just t -> ANFCast id t (ANFVar castretid) cont2
                    let cont4
                          | castArg && castingRet =
                            ANFCast argCast (FTConstr "Int" []) (head argids) $ ANFApp castretid isGlobal ptr (ANFVar closureCast : ANFVar argCast : tail argids) cont3
                          | castArg =
                            ANFCast argCast (FTConstr "Int" []) (head argids) $ ANFApp castretid isGlobal ptr (ANFVar closureCast : ANFVar argCast : tail argids) cont3
                          | castingRet =
                            ANFApp castretid isGlobal ptr (ANFVar closureCast : argids) cont3
                          | otherwise =
                            ANFApp id isGlobal ptr (ANFVar closureCast : argids) cont3
                    return $ ANFCast ptr ft (ANFVar fid) $ ANFCast closureCast (FTConstr "Int" []) (ANFVar fid) cont4
            ANFTuple id conjuncts cont -> closureConvert retCasts cont >>= \t-> return (ANFTuple id conjuncts t)
            ANFProj id tpl idx cont -> closureConvert retCasts cont >>= \t-> return (ANFProj id tpl idx t)
            ANFCast id t v cont -> closureConvert retCasts cont >>= \k-> return (ANFCast id t v k)

hoist :: ANFTerm -> SB (DiffList ANFFunc, DiffList ANFJoin, ANFTerm)
hoist t = case t of
    ANFFunc f args body cont -> do
        (fs, js, body2) <- hoist body
        (fs2, js2, cont2) <- hoist cont
        entry <- fresh $ "entry point join point id for function-being-hoisted " ++ show f
        let fn = HoistedFunc f args (HoistedJoin entry Nothing body2) (js [])
        return (fs . fs2 . (fn:), js2, cont2)
    ANFJoin j mbVal body cont -> do
        (fs, js, body2) <- hoist body
        (fs2, js2, cont2) <- hoist cont
        let jn = HoistedJoin j mbVal body2
        return (fs . fs2, (jn:) . js . js2, cont2)
    ANFHalt _ -> return (id, id, t)
    ANFJump _ _ -> return (id, id, t)
    ANFApp id isGlobal f args cont -> do
        (fs, js, cont2) <- hoist cont
        return (fs, js, ANFApp id isGlobal f args cont2)
    ANFTuple id conjuncts cont -> do
        (fs, js, cont2) <- hoist cont
        return (fs, js, ANFTuple id conjuncts cont2)
    ANFProj id tpl idx cont -> do
        (fs, js, cont2) <- hoist cont
        return (fs, js, ANFProj id tpl idx cont2)
    ANFCast id t v cont -> do
        (fs, js, cont2) <- hoist cont
        return (fs, js, ANFCast id t v cont2)
