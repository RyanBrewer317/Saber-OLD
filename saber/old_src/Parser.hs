module Parser where

import qualified Text.Parsec as P
import Text.Parsec ( (<|>) )
import Grammar ( Syntax(SApp, SInt, SLam, SVar, SLet), STerm(..), Located (..), SyntaxStmt (SFuncDec) )
import qualified Text.Parsec.Pos as P
import Desugar (locate)

type Parser = P.Parsec String ()

run :: String -> Parser a -> String -> Either P.ParseError a
run fn p = P.runParser p () fn

emptyPos :: P.SourcePos
emptyPos = P.newPos "" 0 0

parseVar :: Parser STerm
parseVar = P.try $ do
    pos <- P.getPosition
    first <- P.letter
    rest <- P.many $ P.alphaNum <|> P.char '_'
    return $ locate pos $ SVar $ first : rest

parseIntLit :: Parser STerm
parseIntLit = P.try $ do
    pos <- P.getPosition
    val <- read <$> P.many1 P.digit
    return $ locate pos $ SInt val

parseLambda :: Parser STerm
parseLambda = do
    pos <- P.getPosition
    P.try $ P.string "fn"
    P.spaces
    x <- ((\Loc{val=(SVar x)}->x) <$> P.try (P.between (P.char '(') (P.char ')') (P.spaces *> parseVar <* P.spaces))) <|> ((\()->"_") <$> P.between (P.char '(') (P.char ')') P.spaces)
    P.spaces
    locate pos . SLam x <$> parseTerm

parseLet :: Parser STerm
parseLet = do
    pos <- P.getPosition
    P.try $ P.string "let"
    P.space
    P.spaces
    var <- parseVar
    let Loc {val=(SVar x)} = var
    P.spaces
    P.char '='
    val <- parseTerm
    P.string ";"
    locate pos . SLet x val <$> parseTerm

parseParenthetical :: Parser STerm
parseParenthetical = P.between (P.char '(') (P.char ')') parseTerm

parseTerm :: Parser STerm
parseTerm = do
    P.spaces
    pos <- P.getPosition
    expr <- parseLet <|> P.try parseParenthetical <|> parseLambda <|> parseVar <|> parseIntLit
    P.spaces
    expr <- fold expr . reverse <$> P.many tryCall
    P.spaces
    return expr
    where
        fold :: STerm -> [STerm->STerm]-> STerm
        fold = foldr ($)
        tryCall = do
            pos <- P.getPosition
            call <- parseParenthetical
            P.spaces
            return $ \expr -> locate pos $ SApp expr call

parseFuncDec :: Parser (Located SyntaxStmt)
parseFuncDec = do
    var <- parseVar
    let Loc{start=pos, val=SVar name} = var
    P.spaces
    args <- P.between (P.char '(') (P.char ')') $ P.sepBy (P.spaces *> (parseVar>>= \Loc{val=SVar a}->return a) <* P.spaces) (P.char ',')
    P.spaces
    P.char '='
    body <- parseTerm
    return $ Loc (SFuncDec name args body) pos

parseStmts :: Parser [Located SyntaxStmt]
parseStmts = P.many1 parseFuncDec <* P.eof