{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module ANF where

import Grammar
import Gen
import Data.List (union, foldl')

convert :: Gen -> Term -> (Gen, ANFTerm)
convert gen = \t-> go gen t (\g v->(g, ANFHalt v))
    where
        go gen e k = case e of
            IntLit i -> k gen (ANFInt i)
            Var  x i -> k gen (ANFVar i)
            Lam x i Loc{val=e} ->
                withFresh gen $ \gen2 i2->
                    let (gen3, cont) = k gen2 (ANFVar i2) in
                    let (gen4, e2)   = go gen3 e (\g v->(g, ANFHalt v)) in
                        (gen4, ANFFunc i2 [i] e2 cont)
            App Loc{val=f} Loc{val=a} ->
                go gen f (\gen2 f2->
                go gen2 a (\gen3 a2->
                case f2 of
                    ANFVar i -> withFresh gen3 $ \gen4 i2->
                        let (gen5, cont) = k gen4 (ANFVar i2) in
                        (gen5, ANFApp i2 i [a2] cont)
                    _ -> error "panic!"))
            Let x i locval bod@Loc{val=e,start=pos} -> go gen (App (Loc{val=Lam x i bod,start=pos}) locval) k

fvs :: ANFTerm -> [Int] -> [Int]
fvs t captures = case t of
    ANFHalt (ANFVar i) -> [i | i `notElem` captures]
    ANFHalt _ -> []
    ANFFunc f args body cont -> fvs body (f:args) `union` fvs cont [f]
    ANFJoin n m_n a b -> fvs a [] `union` fvs b []
    ANFJump i m_n -> []
    ANFApp i f args cont -> union (fvs cont [i]) $ ([f | f `notElem` captures]) ++ foldr (\v b->case v of ANFVar i->if i `elem` captures then b else i:b ; _ -> b) [] args
    ANFTuple i conjuncts cont -> union (fvs cont [i]) $ foldr (\v b->case v of ANFVar i->if i `elem` captures then b else i:b ; _ -> b) [] conjuncts
    ANFProj i tpl idx cont -> [tpl | tpl `notElem` captures] `union` fvs cont [i]

closureConvert :: Gen -> ANFTerm -> (Gen, ANFTerm)
closureConvert gen t = case t of
    ANFHalt val -> (gen, t)
    ANFFunc f args body cont -> withFresh gen $ \gen2 env_i->
        let free = filter (`notElem` args) $ fvs body [] in
        let (gen3, body2) = closureConvert gen2 body in
        let body3 = fst (foldl' (\(e, i) x->(ANFProj x env_i i e, i+1)) (body2, 1) free) in
        let (gen4, cont2) = closureConvert gen3 cont in
        withFresh gen4 $ \gen5 i2->
            let cont3 = let vs = map ANFVar free in ANFTuple f (ANFGlob i2:vs) cont2 in
            (gen5, ANFFunc i2 (env_i:args) body3 cont3)
    ANFJoin n m_n a b ->
        let (gen2, a2) = closureConvert gen  a in
        let (gen3, b2) = closureConvert gen2 b in
        (gen3, ANFJoin n m_n a2 b2)
    ANFJump n m_n -> (gen, t)
    ANFApp i f args cont -> 
        withFresh gen $ \gen2 ptr_i->
            let (gen3, cont2) = closureConvert gen2 cont in
            (gen3, ANFProj ptr_i f 0 $ ANFApp i ptr_i (ANFVar f:args) cont2)
    ANFTuple i conjuncts cont -> let (gen2, cont2) = closureConvert gen cont in (gen2, ANFTuple i conjuncts cont2)
    ANFProj i tpl idx cont -> let (gen2, cont2) = closureConvert gen cont in (gen2, ANFProj i tpl idx cont2)

hoist :: Gen -> ANFTerm -> (Gen, ([ANFFunc], [ANFJoin]))
hoist g t = 
    let (gen2, (fs, js, anf)) = go g t in
    withFresh gen2 $ \gen3 i->
        withFresh gen3 $ \gen4 i2->
            let main = ANFFunc i [] anf $ ANFApp i2 i [] $ ANFHalt $ ANFVar i2 in
            let (gen5, (fs2, js2, end)) = go gen4 main in
            (gen5, (fs (fs2 []), js (js2 [])))
    where
        go gen anf = case anf of
            ANFFunc f args body cont -> 
                let (gen2, (fs, js, body2)) = go gen body in
                let (gen3, (fs2, js2, cont2)) = go gen2 cont in
                withFresh gen3 $ \gen4 entry->
                    let fn = HoistedFunc f args (HoistedJoin entry Nothing body2) (js []) in
                    (gen4, (fs . fs2 . (fn:), js2, cont2))
            ANFJoin j p body cont ->
                let (gen2, (fs, js, body2)) = go gen body in
                let (gen3, (fs2, js2, cont2)) = go gen2 cont in
                let jn = HoistedJoin j p body2 in
                (gen3, (fs . fs2, (jn:) . js . js2, cont2))
            ANFHalt val -> (gen, (id, id, anf))
            ANFJump i m_i -> (gen, (id, id, anf))
            ANFApp i f args cont ->
                let (gen2, (fs, js, cont2)) = go gen cont in
                (gen2, (fs, js, ANFApp i f args cont2))
            ANFTuple i conjuncts cont ->
                let (gen2, (fs, js, cont2)) = go gen cont in
                (gen2, (fs, js, ANFTuple i conjuncts cont2))
            ANFProj i tpl idx cont ->
                let (gen2, (fs, js, cont2)) = go gen cont in
                (gen2, (fs, js, ANFProj i tpl idx cont2))
