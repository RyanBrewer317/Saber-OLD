{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
module Main where
import Parser as P
import AST (buildSB)
import Grammar
import Grammar as G (run)
import Typecheck (assignBasicTypes)
import System.Directory
import Specialize (specialize)
import ANF (anf)
-- import MyLLVM (output)
import Imp (output)
import System.Process ( callCommand )
import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import System.Environment (getArgs)

-- | getCode produces the original source code for compilation, 
--   based on the command line arguments
getCode :: [String] -> IO String
getCode args =
    case args of
        [] -> getLine >>= \l -> if null l then return l else ((l ++ "\n") ++) <$> getCode []
        filename:_ -> readFile filename

main :: IO ()
main = do
    args <- getArgs
    s <- getCode args
    case P.run "CLI" parseStmts s of
        Left e -> print e
        Right ast -> do
            -- parsing succeeded, the AST is stored in `ast`.

            -- clear the state of the database
            -- the sqlite database state management is a failed experiment I think, which will get removed in the future.
            removeFile "saber.db"

            -- run the compiler on the AST
            sb_ <- flip runSB SBState{gen=0,sbMain= -1,types=Map.empty,freshData=Map.empty} $ do
                ast <- buildSB ast
                types <- assignBasicTypes ast
                ast <- specialize types ast
                ast <- anf ast
                mod <- output ast
                G.run $ callCommand "clang -w test2.c src/runtime.c"
                G.run $ callCommand "./a.out"
                return ()
            case sb_ of
                Err gen e -> putStrLn e
                Ok state () -> return ()
