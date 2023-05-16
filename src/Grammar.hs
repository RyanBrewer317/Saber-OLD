{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use tuple-section" #-}
module Grammar where
import Database.SQLite.Simple (open, close, query_, execute_, Connection)
import Control.Monad (liftM, ap)
import Text.Parsec (SourcePos)
import qualified Data.Text as Text
import Database.SQLite.Simple.Types (Query(Query))
import Data.IORef (IORef, readIORef)
import qualified Data.Map as Map
import Prelude hiding (fail)

data StmtSyntax = FuncDecSyntax !SourcePos !String !String !ExprSyntax deriving Show

data ExprSyntax = VarSyntax !SourcePos !String
                | IntLitSyntax !SourcePos !Int
                | LamSyntax !SourcePos !String !ExprSyntax
                | AppSyntax !SourcePos !ExprSyntax !ExprSyntax
                | LetSyntax !SourcePos !String !ExprSyntax !ExprSyntax
                deriving Show

data SBState = SBState { sbMain::Int, gen::Int, types::Map.Map Int FBaseType, freshData::Map.Map Int String }

data SB_ a = Err !Int !String | Ok !SBState !a

newtype SB a = SB{ runSB :: SBState -> IO (SB_ a) }

instance Monad SB where
    return x = SB $ \state->return $ Ok state x
    a >>= f = SB $ \state-> do
        res <- runSB a state
        case res of
            Err gen2 s -> return $ Err gen2 s
            Ok state x -> runSB (f x) state

instance Functor SB where
    fmap = liftM

instance Applicative SB where
    pure = return
    (<*>) = ap

fresh :: String -> SB Int
fresh intent = SB $ \state@SBState{gen=gen,freshData=freshData}->putStrLn (show gen ++ ": " ++ intent) >> return (Ok (state{gen=gen + 1, freshData=Map.insert gen intent freshData}) gen)

infixr 4 ?
(?) :: SB a -> String -> SB a
a ? s = SB $ \state-> do
    res <- runSB a state
    case res of
        Err gen2 e -> return $ Err gen2 $ "while " ++ s ++ ", " ++ e
        Ok state x -> return $ Ok state x

run :: IO a -> SB a
run io = SB $ \state-> io >>= \a-> runSB (return a) state

debug :: String -> SB ()
debug s = SB $ \state-> do
    putStrLn s
    runSB (return ()) state

fail :: String -> SB a
fail e = SB $ \SBState{gen=gen}-> return $ Err gen e

sql :: Connection -> String -> SB ()
sql conn s = run $ execute_ conn $ Query $ Text.pack s

setMain :: Int -> SB ()
setMain main = SB $ \state-> return $ Ok state{sbMain=main} ()

getMain :: SB Int
getMain = SB $ \state@SBState{sbMain=main}->return $ Ok state main

setType :: Int -> FBaseType -> SB ()
setType id t = SB $ \state@SBState{types=types}->return $ Ok state{types=Map.insert id t types} ()

getType :: Int -> SB FBaseType
getType id = SB $ \state@SBState{types=types, gen=gen} -> case Map.lookup id types of
    Just t -> return $ Ok state t
    Nothing -> return $ Err gen $ "tried to get type of unassigned index " ++ show id

dumpTypes :: SB ()
dumpTypes = SB $ \state@SBState{types=types} -> Ok state <$> print types

setTypes :: Map.Map Int FBaseType -> SB ()
setTypes ts = SB $ \state-> return $ Ok state{types=ts} ()

substitute :: Int -> FBaseType -> SB ()
substitute i t = SB $ \state@SBState{types=types} ->
    return $ Ok state{types=Map.map (sub i t) types} ()
    where
        sub i t t2 = case t2 of
            FTConstr s ts -> FTConstr s (map (sub i t) ts)
            FTVar id -> if id == i then t else t2
            FForall id t2 -> FForall id $ sub i t t2

getIntent :: Int -> SB (Maybe String)
getIntent i = SB $ \state@SBState{freshData=freshData}->return $ Ok state $ Map.lookup i freshData

data Stmt = FuncDec !Int !Int !Expr deriving Show

data Expr = Var !Int !Bool !Int
          | IntLit !Int
          | Lam !Int !Int !Expr
          | App !Int !Expr !Expr
          | Let !Int !Int !Expr !Expr
          deriving Show

getId expr = case expr of
    Var id _ _ -> id
    IntLit id -> id
    Lam id _ _ -> id
    App id _ _ -> id
    Let id _ _ _ -> id

data BaseType = Forall !(IORef BaseType) !(IORef BaseType)
              | TVar !Int
              | TConstr !String ![IORef BaseType]

data FBaseType = FForall !Int !FBaseType
               | FTVar !Int
               | FTConstr !String ![FBaseType]
               deriving Show

freeze :: IORef BaseType -> IO FBaseType
freeze bt = readIORef bt >>= \bt->case bt of
    Forall vio tio -> readIORef vio >>= \v-> case v of
        TVar i -> FForall i <$> freeze tio
        _ -> freeze tio
    TVar i -> return $ FTVar i
    TConstr s ios -> FTConstr s <$> mapM freeze ios

strbasetype :: IORef BaseType -> IO String
strbasetype btio = readIORef btio >>= \bt->case bt of
    Forall vio tio -> strbasetype tio
    TVar id -> return $ "t"++show id
    TConstr s ios -> mapM strbasetype ios >>= \ss->return $ s ++ (if not (null ios) then " " else "") ++ unwords ss

data ANFVal = ANFVar !Int
            | ANFInt !Int
            | ANFGlobal !Int
            deriving Show

data ANFTerm = ANFHalt !ANFVal
             | ANFFunc !Int ![Int] !ANFTerm !ANFTerm
             | ANFJoin !Int !(Maybe Int) !ANFTerm !ANFTerm
             | ANFJump !Int !(Maybe Int)
             | ANFApp !Int !Bool !Int ![ANFVal] !ANFTerm -- the bool is for if the second int refers to a global function
             | ANFTuple !Int ![ANFVal] !ANFTerm
             | ANFProj !Int !Int !Int !ANFTerm
             | ANFCast !Int !FBaseType !ANFVal !ANFTerm
             deriving Show

data ANFJoin = HoistedJoin !Int !(Maybe Int) !ANFTerm deriving Show
data ANFFunc = HoistedFunc !Int ![Int] !ANFJoin ![ANFJoin] deriving Show

type DiffList a = [a] -> [a]
