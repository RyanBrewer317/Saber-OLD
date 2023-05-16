{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Main where

import ANF
import Data.List (intercalate)
import Desugar (alpha, alphaStmts)
import Gen
import Grammar
import Parser (parseTerm, run, parseStmts)
import Data.Maybe (fromMaybe)
import MyLLVM
import Typecheck
import Control.Monad.Trans.Except (runExceptT)
import LLVM.Pretty (ppllvm)
import Data.Text.Lazy (unpack)
import System.Process ( callCommand )

getCode :: IO String
getCode = getLine >>= \l -> if null l then return l else (l ++) <$> getCode

pprint :: Located Term -> String
pprint Loc {val = t} = case t of
        Var x i -> x ++ show i
        IntLit i -> show i
        Lam x i e -> "\\" ++ x ++ show i ++ ". " ++ pprint e
        App f a -> pprint f ++ "(" ++ pprint a ++ ")"
        Let x i v e -> "let " ++ x ++ show i ++ " = " ++ pprint v ++ "; " ++ pprint e

pprintANFVal :: ANFVal -> String
pprintANFVal v = case v of
        ANFVar n -> "x" ++ show n
        ANFInt n -> show n
        ANFGlob n -> "x"++show n

pprintANF :: ANFTerm -> String
pprintANF t = case t of
        ANFHalt v -> "halt(" ++ pprintANFVal v ++ ")"
        ANFFunc n ns at cont -> "func x" ++ show n ++ "(" ++ intercalate ", " (map (\i -> "x" ++ show i) ns) ++ ") = " ++ pprintANF at ++ " in " ++ pprintANF cont
        ANFJoin n m_n at at' -> ""
        ANFJump n m_n -> ""
        ANFApp n i args cont -> "let x" ++ show n ++ " = x" ++ show i ++ "(" ++ intercalate ", " (map pprintANFVal args) ++ ") in " ++ pprintANF cont
        ANFTuple i conjuncts cont -> "let x" ++ show i ++ " = (" ++ intercalate ", " (map pprintANFVal conjuncts) ++ ") in " ++ pprintANF cont
        ANFProj i tpl idx cont -> "let x" ++ show i ++ " = x" ++ show tpl ++ "[" ++ show idx ++ "] in " ++ pprintANF cont

instance Show ANFTerm where
        show = pprintANF

instance Show ANFFunc where
        show (HoistedFunc f as j js) = "func x"++show f++"("++intercalate ", " (map (("x"++).show) as)++"){"++show j++"}"

instance Show ANFJoin where
        show (HoistedJoin j m_i t) = "join x"++show j++"("++maybe "" (\i->"x"++show i) m_i++"){"++show t++"}"

main :: IO ()
main = do
        s <- getCode
        case run "CLI" parseStmts s of
                Left e -> print e
                Right stmts -> case alphaStmts Gen.init stmts of
                        Left e -> putStrLn e
                        Right (gen, (stmts, _)) ->
                                let resIO = runExceptT $ algoJStmts gen TCNil stmts in
                                resIO >>= \res->case res of
                                        Left e -> putStrLn e
                                        Right (_, stmts) ->
                                                let (gen2, anf) = convert gen a in
                                                let (gen3, anf2) = closureConvert gen2 anf in
                                                let (gen4, (fs, js)) = hoist gen3 anf2 in do
                                                -- putStrLn $ intercalate "\n\n" (map show fs)
                                                let (gen5, mod) = output gen4 fs js
                                                let str = unpack $ ppllvm mod
                                                writeFile "cli.ll" str
                                                callCommand "clang cli.ll src/runtime.c"
                                                callCommand "./a.out"
        