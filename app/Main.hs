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

getCode :: String -> IO String
getCode filename = 
    if null filename then
        getLine >>= \l -> if null l then return l else ((l ++ "\n") ++) <$> getCode ""
    else
        readFile filename

main :: IO ()
main = do
    args <- getArgs
    s <- getCode $ head args
    case P.run "CLI" parseStmts s of
        Left e -> print e
        Right ast -> do
            removeFile "saber.db"
            sb_ <- runSB (do
                ast <- buildSB ast
                types <- assignBasicTypes ast
                ast <- specialize types ast
                ast <- anf ast
                mod <- output ast
                -- let str = TL.unpack $ ppllvm mod
                -- G.run $ writeFile "cli.ll" str
                -- G.run $ callCommand "clang cli.ll src/runtime.c"
                -- G.run $ callCommand "./a.out"
                return ()) SBState{gen=0,sbMain= -1,types=Map.empty,freshData=Map.empty}
            case sb_ of
                Err gen e -> putStrLn e
                Ok state () -> return ()
