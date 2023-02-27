{-# LANGUAGE InstanceSigs #-}
module Grammar where
import Text.Parsec (SourcePos)
import Data.IORef (IORef)

data Syntax = SVar !String
            | SInt !Int
            | SLam !String !STerm
            | SApp !STerm !STerm
            | SLet !String !STerm !STerm
            deriving (Show)

type STerm = Located Syntax

data Located a = Loc { val :: !a, start :: !SourcePos } deriving (Show)

data Term = Var !String !Int
          | IntLit !Int
          | Lam !String !Int !(Located Term)
          | App !(Located Term) !(Located Term)
          | Let !String !Int !(Located Term) !(Located Term)
          deriving (Show)

data ANFVal = ANFVar !Int
            | ANFInt !Int
            | ANFGlob !Int

data ANFTerm = ANFHalt !ANFVal
             | ANFFunc !Int ![Int] !ANFTerm !ANFTerm
             | ANFJoin !Int !(Maybe Int) !ANFTerm !ANFTerm
             | ANFJump !Int !(Maybe Int)
             | ANFApp !Int !Int ![ANFVal] !ANFTerm
             | ANFTuple !Int ![ANFVal] !ANFTerm
             | ANFProj !Int !Int !Int !ANFTerm

data ANFJoin = HoistedJoin !Int !(Maybe Int) !ANFTerm
data ANFFunc = HoistedFunc !Int ![Int] !ANFJoin ![ANFJoin]

type DiffList a = [a] -> [a]

instance Show (IORef a) where
    show :: IORef a -> String
    show ir = "ioref"

data TempMonotype = TempTVar !(IORef (Located TempMonotype)) | TempTConstr !String ![Located TempMonotype] | Uninstantiated !Int deriving Show

data TempPolyType = TempMono !TempMonotype | TempForall !(IORef (Located TempMonotype)) !TempPolyType deriving Show

data TempTerm = TempVar !String !Int
              | TempInt !Int
              | TempLam !String !Int !(TempTypedLoc TempTerm)
              | TempApp !(TempTypedLoc TempTerm) !(TempTypedLoc TempTerm)
              | TempLet !String !Int !(TempTypedLoc TempTerm) !(TempTypedLoc TempTerm)
              deriving Show

data TempContext = TCNil | !TempContext :> !(Int, TempPolyType)
member :: Int -> TempContext ->  Bool
member x gamma = case gamma of
    TCNil -> False
    gamma2 :> (i, _) -> (x == i) || (x `member` gamma2)
(|-) :: TempContext -> Int -> Maybe TempPolyType
gamma |- x = case gamma of
    TCNil -> Nothing
    gamma2 :> (i, t) -> if x == i then Just t else gamma2 |- x

data TempTypedLoc a = TTLoc {ttval :: !a, ttstart :: !SourcePos, ttype :: !TempPolyType} deriving Show
locToTT :: TempPolyType ->Located a ->  TempTypedLoc a
locToTT t Loc{val=v, start=s} = TTLoc{ttval=v, ttstart=s, ttype=t}