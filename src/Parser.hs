module Parser where
import Grammar
import Text.Parsec as P
import Data.Maybe (fromMaybe)
import Data.Functor (($>))

-- | Parser type for Saber parser combinators.
type Parser = P.Parsec String ()

-- | Turn a parser @p: Parser a@ into a function from source code to either an error or the result of the parse.
--   The filename @fn: String@ is used as the name of the source for parse-error purposes; no I/O happens here of course.
run :: String -> Parser a -> (String -> Either P.ParseError a)
run fn p = P.runParser p () fn

-- | parser for reserved words. Makes sure no identifier characters follow the reserved word.
reserved :: String -> Parser String
reserved s = P.string s <* P.notFollowedBy (P.alphaNum <|> P.char '_')

-- | parser for whitespace and, in the future, comments
ws :: Parser ()
ws = P.spaces

-- | Parse an identifier according to the Saber grammar: start with a lowercase letter, then any letters, digits, or @_@.
--   This doesn't check against reserved words; the caller must handle this.
parseVarString :: Parser String
parseVarString = P.try $ do
    first <- P.letter
    rest <- P.many $ P.alphaNum <|> P.char '_'
    return $ first : rest

-- | Parse an identifier according to the Saber grammar: start with lowercase letter, then any letters, digits, or @_@.
--   If the identifier is followed by @=@ then it is reinterpreted as a let-binding and parsed as such.
--   This doesn't check against reserved words so parsers for those constructs have to happen before this in a @choice@.
parseVarOrLet :: Parser ExprSyntax
parseVarOrLet = do
    pos <- P.getPosition
    x <- parseVarString <?> "identifier"
    ws
    mbLet <- P.optionMaybe (P.string "=" >> parseTerm >>= \v-> P.char ';' >> parseTerm >>= \e-> return (v, e)) <?> "binder"
    case mbLet of
        Nothing -> {- parse mutation, then if that fails -} return $ VarSyntax pos x
        Just (v, e) -> return $ LetSyntax pos x v e

-- | parse an integer literal.
parseInt :: Parser ExprSyntax
parseInt = P.try $ do
    pos <- P.getPosition
    val <- read <$> P.many1 P.digit <?> "integer literal"
    return $ IntLitSyntax pos val

-- | parse a lambda literal @fn(x)e@.
parseLambda :: Parser ExprSyntax
parseLambda = do
    pos <- P.getPosition
    P.try (reserved "fn" <?> "lambda")
    ws
    P.char '('
    ws
    mbArg <- P.optionMaybe parseVarString <?> "argument"
    let arg = fromMaybe "_" mbArg
    ws
    P.char ')'
    LamSyntax pos arg <$> parseTerm

-- | parse an expression that's in parentheses.
parseParenthetical :: Parser ExprSyntax
parseParenthetical = P.between (P.char '(') (P.char ')') parseTerm

-- | parse an expression
parseTerm :: Parser ExprSyntax
parseTerm = do
    ws
    P.notFollowedBy $ reserved "def"
    pos <- P.getPosition
    expr <- (parseLambda <|> parseVarOrLet <|> parseInt <|> parseParenthetical) <?> "expression"
    ws
    expr <- fold expr . reverse <$> P.many tryCall -- todo: is the reverse necessary? At the moment, application right-associates which is not pleasant
    ws
    return expr
    where
        fold = foldr ($)
        tryCall = do
            pos <- P.getPosition
            call <- parseTerm <?> "function call"
            ws
            return $ \expr -> AppSyntax pos expr call

-- | parser for a single statement, until more statements exist than just function declarations (enums, etc.).
parseFuncDec :: Parser StmtSyntax
parseFuncDec = do
    pos <- P.getPosition
    ws
    P.try (reserved "def" <?> "function declaration")
    ws
    name <- parseVarString
    ws
    args <- P.many $ parseVarString <* ws
    P.char ':'
    e <- parseTerm
    case args of
        arg : rest -> do
            e <- return $ foldr (LamSyntax pos) e rest
            return $ FuncDecSyntax pos name arg e
        [] -> return $ FuncDecSyntax pos name "_" e

-- | The top level parser that produces a list of statements given the source code.
parseStmts :: Parser [StmtSyntax]
parseStmts = P.many1 parseFuncDec <* P.eof