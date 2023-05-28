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

-- | This is the type of statements immediately parsed. They have all (and only) the information from the parsing pass, directly in their fields. 
--   Don't use this outside of parsing and building the AST. Even typechecking is too late in the pipeline for this type.
data StmtSyntax = 
    -- | FuncDecSyntax [position in source file] [name] [arg name] [body]
    FuncDecSyntax !SourcePos !String !String !ExprSyntax deriving Show

-- | This is the type of expressions immediately parsed. They have all (and only) the information from the parsing pass, directly in their fields. 
--   Don't use this outside of parsing and building the AST. Even typechecking is too late in the pipeline for this type.
data ExprSyntax 
    -- | VarSyntax [position in source file] [name]
    = VarSyntax !SourcePos !String
    -- | IntLitSyntax [position in source file] [value]
    | IntLitSyntax !SourcePos !Int
    -- | LamSyntax [position in source file] [arg name] body
    | LamSyntax !SourcePos !String !ExprSyntax
    -- | AppSyntax [position in source file] [func] [arg]
    | AppSyntax !SourcePos !ExprSyntax !ExprSyntax
    -- | LetSyntax [position in source file] [var name] [value] [scope/rest]
    | LetSyntax !SourcePos !String !ExprSyntax !ExprSyntax
    deriving Show

-- | Saber's state type, for information that is mutated and looked up during the process of compilation, in an imperative sort of way.
--   For example, @gen@ holds the value of the next integer returned by @fresh@
data SBState = SBState { sbMain::Int, gen::Int, types::Map.Map Int FBaseType, freshData::Map.Map Int String }

-- | Saber's result type. The error holds an int just so the fresh-name-generator can be thought of as linearly typed :-)
--   The error value is a string produced by a series of string concatenations; I intend to improve this someday to be more structured and less lossy.
data SB_ a = 
    -- | The constructor for errors in Saber. Use @fail@ unless messing with the internals of Saber's SB monad. 
    --   The int is from the "linearly-typed" (in spirit) fresh integer generator. 
    --   The string is the error message. In the future this will be better than just a string.
      Err !Int !String 
    -- | The constructor for values that don't fail in Saber. The "mutable" state (in a State monad sort of mutability) is passed alongside these values.
    --   In general you don't see this constructor, it's just an implementation detail of Saber's SB monad, and for checking if the SB computation failed. 
    --   Use @return@ in general for nonfailing values in Saber.
    | Ok !SBState !a

-- | Saber's custom monad. This supports I/O, mutable state, and exception handling; 
--   Saber's implementation is properly multi-paradigm, and when bootstrapping this will be more efficient.
newtype SB a = 
    -- | The constructor for Saber's main monad, SB. This is just seen in the internals of Saber's sort of DSL.
    --   For error handling, use @fail@ and @?@, and check for errors with the @Err@ and @Ok@ constructors.
    --   For doing I/O, use @run@, @debug@, @dumpTypes@, and @sql@.
    --   For doing mutable state, use the various getters and setters exposed by Grammar.hs: 
    --   @fresh@ and @getIntent@ for intentions and fresh names;
    --   @setType@, @getType@, @substitute@, and @setTypes@ for types; and
    --   @setMain@ and @getMain@ for the id of the program's main function.
    SB{ runSB :: SBState -> IO (SB_ a) }

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

-- | Produce an integer that hasn't been produced by @fresh@ before. 
--   @fresh@ takes a string that is the "intent" of this integer, and stores the intent for later retrieval by @getIntent@.
fresh :: String -> SB Int
fresh intent = SB $ \state@SBState{gen=gen,freshData=freshData}->putStrLn (show gen ++ ": " ++ intent) >> return (Ok (state{gen=gen + 1, freshData=Map.insert gen intent freshData}) gen)

-- | Given an expression, if it fails, add information to the beginning of the error string.
--   This is used as a sort of runtime commenting system: @e?c@ does nothing but e, and also produces better errors.
--   The infix level of 4 allows expressions like @e?c++d@ to be interpreted as @e?(c++d)@ which is very handy.
infixr 4 ? 
(?) :: SB a -> String -> SB a
a ? s = SB $ \state-> do
    res <- runSB a state
    case res of
        Err gen2 e -> return $ Err gen2 $ "while " ++ s ++ ", " ++ e
        Ok state x -> return $ Ok state x

-- | Lift an IO action into the SB monad.
run :: IO a -> SB a
run io = SB $ \state-> io >>= \a-> runSB (return a) state

-- | Print a string to stdio.
debug :: String -> SB ()
debug s = SB $ \state-> do
    putStrLn s
    runSB (return ()) state

-- | Throw an error, in the SB monad's error-handling sense.
fail :: String -> SB a
fail e = SB $ \SBState{gen=gen}-> return $ Err gen e

-- | Execute a SQL query @s: String@ on the connection @conn: Connection@.
sql :: Connection -> String -> SB ()
sql conn s = run $ execute_ conn $ Query $ Text.pack s

-- | Store the id of the program's main function until later.
setMain :: Int -> SB ()
setMain main = SB $ \state-> return $ Ok state{sbMain=main} ()

-- | Retrieve the id of the program's main function. -1 indicates that no id has been stored for the main function.
getMain :: SB Int
getMain = SB $ \state@SBState{sbMain=main}->return $ Ok state main

-- | Set the type of the given id to the given type.
setType :: Int -> FBaseType -> SB ()
setType id t = SB $ \state@SBState{types=types}->return $ Ok state{types=Map.insert id t types} ()

-- | Retrieve the type of the given id.
getType :: Int -> SB FBaseType
getType id = SB $ \state@SBState{types=types, gen=gen} -> case Map.lookup id types of
    Just t -> return $ Ok state t
    Nothing -> return $ Err gen $ "tried to get type of unassigned index " ++ show id

-- | Print all id-type pairs stored (for debugging purposes).
dumpTypes :: SB ()
dumpTypes = SB $ \state@SBState{types=types} -> let (SB f) = debug (show types) in f state -- this is done so that `debug` is used for all printing

-- | Overwrite the entire storage of id-type pairs to a given mapping.
setTypes :: Map.Map Int FBaseType -> SB ()
setTypes ts = SB $ \state-> return $ Ok state{types=ts} ()

-- | replace all stored type variables of the given type-variable-id with the given type
substitute :: Int -> FBaseType -> SB ()
substitute i t = SB $ \state@SBState{types=types} ->
    return $ Ok state{types=Map.map (sub i t) types} ()
    where
        sub i t t2 = case t2 of
            FTConstr s ts -> FTConstr s (map (sub i t) ts)
            FTVar id -> if id == i then t else t2
            FForall id t2 -> FForall id $ sub i t t2

-- | Return the declared intention of a given integer that was produced by a call to @fresh@.
getIntent :: Int -> SB (Maybe String)
getIntent i = SB $ \state@SBState{freshData=freshData}->return $ Ok state $ Map.lookup i freshData

-- | The type of statements after AST building and before ANF conversion. Basically the static analysis phase.
data Stmt = 
    -- | FuncDec [function id] [arg id] [body]
    FuncDec !Int !Int !Expr deriving Show

-- | The type of expressions after AST building and before ANF conversion. Basically the static analysis phase.
data Expr
    -- | Var [literal id] [isGlobal] [id of var, in the sense that all instances of a var have some shared identity]
    = Var !Int !Bool !Int
    -- | IntLit [int id NOT VALUE] (find the value in the sql database)
    | IntLit !Int
    -- | Lam [lam id] [var id] [body]
    | Lam !Int !Int !Expr
    -- | App [app id] [func] [arg]
    | App !Int !Expr !Expr
    -- | Let [let id] [var id] [val] [scope/rest]
    | Let !Int !Int !Expr !Expr
    deriving Show

-- | Get the id of a given expression.
getId :: Expr -> Int
getId expr = case expr of
    Var id _ _ -> id
    IntLit id -> id
    Lam id _ _ -> id
    App id _ _ -> id
    Let id _ _ _ -> id

-- | Mutable types (for alg-J). These are then frozen after type inference, into @FBaseType@. 
--   This will likely be changed in a future refactor. 
data BaseType 
    -- | Forall [IORef to type var] [IORef to rest of scheme]. 
    --   If the type var is not a type var, that type var has been instantiated and the Forall can be ignored.
    = Forall !(IORef BaseType) !(IORef BaseType)
    -- | TVar [type variable id]
    | TVar !Int
    -- | TConstr [name] [list of IORefs to type arguments]
    | TConstr !String ![IORef BaseType]

-- | Frozen types after mutability was used. 
--   This is used after alg-J, for calculating size types and the types used in the generated C code.
data FBaseType 
    -- | FForall [type var id] [rest of scheme]
    = FForall !Int !FBaseType
    -- | FTVar [type var id]
    | FTVar !Int
    -- | FTConstr [name] [list of type args]
    | FTConstr !String ![FBaseType]
    deriving Show

-- | Turn mutable types into immutable ones. Hangs if the IORefs erroneously form a cycle.
freeze :: IORef BaseType -> IO FBaseType
freeze bt = readIORef bt >>= \bt->case bt of
    Forall vio tio -> readIORef vio >>= \v-> case v of
        TVar i -> FForall i <$> freeze tio
        _ -> freeze tio
    TVar i -> return $ FTVar i
    TConstr s ios -> FTConstr s <$> mapM freeze ios

-- | Produce a string representation of a mutable type. Hangs if the IORefs erroneously form a cycle.
strbasetype :: IORef BaseType -> IO String
strbasetype btio = readIORef btio >>= \bt->case bt of
    Forall vio tio -> strbasetype tio
    TVar id -> return $ "t"++show id
    TConstr s ios -> mapM strbasetype ios >>= \ss->return $ s ++ (if not (null ios) then " " else "") ++ unwords ss

-- | Values in the ANF IR.
data ANFVal 
    -- | ANFVar [var id]
    = ANFVar !Int
    -- | ANFInt [value]
    | ANFInt !Int
    -- | ANFGlobal [var id]
    | ANFGlobal !Int
    deriving Show

-- | The type of expressions in the ANF IR.
data ANFTerm 
    -- | ANFHalt [return value]
    = ANFHalt !ANFVal
    -- | ANFFunc [func id] [list of arg ids] [body] [continuation]
    | ANFFunc !Int ![Int] !ANFTerm !ANFTerm
    -- | ANFJoin [join id] [id of spilled value] [body] [continuation]
    | ANFJoin !Int !(Maybe Int) !ANFTerm !ANFTerm
    -- | ANFJump [jump id] [id of spilled value]
    | ANFJump !Int !(Maybe Int)
    -- | ANFApp [app id] [func-is-global] [func id] [list of arguments] [continuation]
    | ANFApp !Int !Bool !Int ![ANFVal] !ANFTerm
    -- | ANFTuple [tuple id] [list of vals] [continuation]
    | ANFTuple !Int ![ANFVal] !ANFTerm
    -- | ANFProj [projection id] [tuple id] [index] [continuation]
    | ANFProj !Int !Int !Int !ANFTerm
    -- | ANFCast [cast id] [new type] [value] [continuation]
    | ANFCast !Int !FBaseType !ANFVal !ANFTerm
    deriving Show

-- | Join points hoisted to the top level of functions and the program.
data ANFJoin
    -- | HoistedJoin [join id] [spill] [body]
    = HoistedJoin !Int !(Maybe Int) !ANFTerm deriving Show
-- | Functions hoisted to the top level of the program.
data ANFFunc 
    -- | HoistedFunc [function id] [list of arg ids] [entry join point] [other join points]
    = HoistedFunc !Int ![Int] !ANFJoin ![ANFJoin] deriving Show

-- | Lists represented as append-functions, so all the actual O(N) appends happen once at the end by supplying an empty list.
type DiffList a = [a] -> [a]
