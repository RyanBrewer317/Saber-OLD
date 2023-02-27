module Parser where

import qualified Text.Parsec as P
import Text.Parsec ( (<|>) )
import Grammar ( Syntax(SApp, SInt, SLam, SVar, SLet), STerm(..), Located (..) )
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
    t <- parseLet <|> P.try parseParenthetical <|> parseLambda <|> parseVar <|> parseIntLit
    P.spaces
    mbCall <- P.optionMaybe parseParenthetical
    P.spaces
    return $ case mbCall of
            Nothing -> t
            Just arg -> locate pos $ SApp t arg