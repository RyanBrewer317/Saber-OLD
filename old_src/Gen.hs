module Gen where

newtype Gen = Gen Int deriving Show

init :: Gen
init = Gen 0

withFresh :: Gen -> (Gen -> Int -> (Gen, a)) -> (Gen, a)
withFresh gen f = case gen of
    Gen i -> 
        let newvar = i + 1 in
        let newgen = Gen (i + 1) in
        f newgen newvar

withFreshM :: Gen -> (Gen -> Int -> m (Gen, a)) -> m (Gen, a)
withFreshM gen f = case gen of
    Gen i ->
        let newvar = i + 1 in
        let newgen = Gen (i + 1) in
        f newgen newvar