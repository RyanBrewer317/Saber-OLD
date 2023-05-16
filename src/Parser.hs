module Parser where
import Grammar
import Text.Parsec as P
import Data.Maybe (fromMaybe)

type Parser = P.Parsec String ()

run :: String -> Parser a -> String -> Either P.ParseError a
run fn p = P.runParser p () fn

reserved :: String -> Parser String
reserved s = P.string s <* P.notFollowedBy (P.alphaNum <|> P.char '_')

ws :: Parser ()
ws = P.spaces

parseVarString :: Parser String
parseVarString = P.try $ do
    first <- P.letter
    rest <- P.many $ P.alphaNum <|> P.char '_'
    return $ first : rest

parseVarOrLet :: Parser ExprSyntax
parseVarOrLet = do
    pos <- P.getPosition
    x <- parseVarString <?> "identifier"
    ws
    mbLet <- P.optionMaybe (P.string "=" >> parseTerm >>= \v-> P.char ';' >> parseTerm >>= \e-> return (v, e)) <?> "binder"
    case mbLet of
        Nothing -> {- parse mutation, then if that fails -} return $ VarSyntax pos x
        Just (v, e) -> return $ LetSyntax pos x v e

parseInt :: Parser ExprSyntax
parseInt = P.try $ do
    pos <- P.getPosition
    val <- read <$> P.many1 P.digit <?> "integer literal"
    return $ IntLitSyntax pos val

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

parseParenthetical :: Parser ExprSyntax
parseParenthetical = P.between (P.char '(') (P.char ')') parseTerm

parseTerm :: Parser ExprSyntax
parseTerm = do
    ws
    P.notFollowedBy $ reserved "def"
    pos <- P.getPosition
    expr <- (parseLambda <|> parseVarOrLet <|> parseInt <|> parseParenthetical) <?> "expression"
    ws
    expr <- fold expr . reverse <$> P.many tryCall
    ws
    return expr
    where
        fold = foldr ($)
        tryCall = do
            pos <- P.getPosition
            call <- parseTerm <?> "function call"
            ws
            return $ \expr -> AppSyntax pos expr call

parseFuncDec :: Parser StmtSyntax
parseFuncDec = do
    pos <- P.getPosition
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

parseStmts :: Parser [StmtSyntax]
parseStmts = P.many1 parseFuncDec <* P.eof