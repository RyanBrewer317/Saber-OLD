{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE InstanceSigs #-}
module Typecheck where
import Grammar
    ( (|-),
      Located(..),
      TempContext((:>)),
      TempMonotype(..),
      TempPolyType(..),
      TempTerm(TempLet, TempVar, TempInt, TempApp),
      TempTypedLoc(..),
      Term(..), TempStmt (..), Stmt (..) )
import Gen ( withFreshM, Gen, withFresh )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Monad.Trans.Except ( ExceptT, throwE )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Data.List (intercalate)
import Data.Foldable (foldrM)

class TempType a where
    copy :: Located a -> IO (Located a)

instance TempType TempPolyType where
    copy :: Located TempPolyType -> IO (Located TempPolyType)
    copy lpt = case val lpt of
      TempMono tm -> copy (Loc tm $ start lpt) >>= \cp-> return $ Loc (TempMono $ val cp) (start cp)
      TempForall ir tpt -> copy (Loc tpt $ start lpt) >>= \cp-> readIORef ir >>= copy >>= newIORef >>= \cp2-> return $ Loc (TempForall cp2 $ val cp) $ start cp

instance TempType TempMonotype where
    copy :: Located TempMonotype -> IO (Located TempMonotype)
    copy lmt = case val lmt of
        TempTVar ir -> readIORef ir >>= copy >>= newIORef >>= \cp-> return $ Loc (TempTVar cp) $ start lmt
        TempTConstr s tms -> mapM copy tms >>= \cp-> return $ Loc (TempTConstr s cp) $ start lmt
        Uninstantiated i -> return $ Loc (Uninstantiated i) $ start lmt

instantiateNoCopy :: Gen -> Located TempPolyType -> IO (Gen, Located TempPolyType)
instantiateNoCopy gen lpt = case val lpt of
    TempMono tm -> return (gen, lpt)
    TempForall ir tpt -> withFreshM gen $ \gen2 i-> writeIORef ir (Loc (Uninstantiated i) $ start lpt) >> instantiateNoCopy gen2 (Loc tpt $ start lpt)

instantiate :: Gen -> Located TempPolyType -> IO (Gen, Located TempPolyType)
instantiate gen pt = copy pt >>= instantiateNoCopy gen

algoJStmt :: Gen -> TempContext -> Located Stmt -> ExceptT String IO (Gen, (TempContext, Located TempStmt))
algoJStmt gen gamma loc@Loc{val=stmt, start=pos} = case stmt of
    FuncDec n i args locB -> do
        (gen2, typedArgs) <- lift $ foldrM (\(a, i2) (genx, ta)->withFreshM genx $ \genx2 ti->newIORef (Loc (Uninstantiated ti) pos) >>= \t->return (genx2, (TempMono $ TempTVar t, a, i2):ta)) (gen, []) args
        let gamma2 = foldr (\(t, a, i2) gammax->gammax :> (i2, t)) gamma typedArgs
        (gen3, ttlb) <- algoJ gen2 gamma2 locB
        return (gen3, (gamma :> (i, (\((t, _, _):_)->t) typedArgs ), Loc{val=TempFuncDec n i typedArgs ttlb, start=pos}))

algoJStmts :: Gen -> TempContext -> [Located Stmt] -> ExceptT String IO (Gen, [Located TempStmt])
algoJStmts gen gamma locs = case locs of
    [] -> return (gen, [])
    loc:rest -> do
        (gen2, (gamma2, loc2)) <- algoJStmt gen gamma loc
        (gen3, rest2) <- algoJStmts gen2 gamma2 rest
        return (gen3, loc2:rest2)

algoJ :: Gen -> TempContext -> Located Term -> ExceptT String IO (Gen, TempTypedLoc TempTerm)
algoJ gen gamma loc@Loc{val=t, start=pos} = case t of
    Var s n -> case gamma |- n of
        Nothing -> error "undefined var during algoJ"
        Just pt -> do
            (gen2, pt2) <- lift (instantiate gen (Loc pt $ start loc))
            return (gen2, TTLoc (TempVar s n) pos $ val pt2)
    IntLit n -> return (gen, TTLoc (TempInt n) pos (TempMono $ TempTConstr "Int" []))
    Lam s n locE@Loc{val=e} -> withFreshM gen $ \gen2 i-> do
        t <- lift $ newIORef $ Loc (Uninstantiated i) $ start loc
        (gen2, ttlt) <- algoJ gen (gamma :> (n, TempMono $ TempTVar t)) locE
        let TTLoc{ttype=TempMono ty, ttval=tl} = ttlt
        return (gen2, TTLoc tl pos (TempMono $ TempTConstr "->" [Loc (TempTVar t) $ start loc, Loc ty $ start locE]))
    App locF@Loc{val=f} locA@Loc{val=a} -> do
        (gen2, ttltF) <- algoJ gen  gamma locF
        (gen3, ttltA) <- algoJ gen2 gamma locA
        withFreshM gen3 $ \gen4 i-> do
            let TTLoc{ttype=TempMono tf} = ttltF
            let TTLoc{ttype=TempMono ta} = ttltA
            ir <- lift (newIORef $ Loc (Uninstantiated i) $ start locF)
            let outT = TempTVar ir
            unify (Loc{val=TempTConstr "->" [Loc ta $ start locA, Loc outT $ start locF], start=start locF}) Loc{val=tf, start=start locF}
            return (gen4, TTLoc (TempApp ttltF ttltA) pos (TempMono outT))
    Let s n locV@Loc{val=v} locE@Loc{val=e} -> do
        (gen2, ttltV) <- algoJ gen gamma locV
        let TTLoc{ttype=tv} = ttltV
        tv2 <- lift (generalize $ Loc tv $ ttstart ttltV)
        (gen3, ttltE) <- algoJ gen2 (gamma :> (n, tv2)) locE
        let TTLoc{ttype=te} = ttltE
        return (gen3, TTLoc (TempLet s n ttltV ttltE) pos te)

unify :: Located TempMonotype -> Located TempMonotype -> ExceptT String IO ()
unify lt1 lt2 =
    let Loc{val=t1, start=pos} = lt1 in
    let Loc{val=t2} = lt2 in
    case t1 of
        TempTVar ir -> do
            mt <- lift $ readIORef ir
            case val mt of
                Uninstantiated _ -> lift $ writeIORef ir lt2
                _ -> unify mt lt2
        TempTConstr s args ->
            case t2 of
                TempTVar ir -> do
                    mt <- lift $ readIORef ir
                    case val mt of
                        Uninstantiated _ -> lift $ writeIORef ir lt1
                        _ -> unify lt1 mt
                TempTConstr s2 args2 ->
                    if s == s2 && length args == length args2 then
                        mapM_ (uncurry unify) (zip args args2)
                    else throwE $ show pos ++ ": can't unify " ++ s ++ " and " ++ s2
                Uninstantiated _ -> undefined
        Uninstantiated _ -> undefined

collectFreeVars :: Located TempMonotype -> IO [IORef (Located TempMonotype)]
collectFreeVars mt =
    case val mt of
        TempTVar ir -> readIORef ir >>= \t->case val t of
            TempTVar _ -> collectFreeVars t
            TempTConstr _ mts -> mapM collectFreeVars mts >>= return . concat
            Uninstantiated n -> return [ir]
        TempTConstr _ mts -> mapM collectFreeVars mts >>= return . concat
        Uninstantiated _ -> undefined

generalize :: Located TempPolyType -> IO TempPolyType
generalize pt = copy pt >>= \cp-> case val cp of
    TempMono mt -> collectFreeVars (Loc mt (start cp)) >>= \vars->return $ foldr TempForall (TempMono mt) vars
    TempForall ir pt2 -> generalize Loc{val=pt2, start=start cp}