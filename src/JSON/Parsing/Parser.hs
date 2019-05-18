module JSON.Parsing.Parser
    ( parseFile
    )
where

import           Text.ParserCombinators.Parsec
import           JSON.Parsing.AST
import qualified JSON.Parsing.Lexer            as L
import qualified Data.Map                      as M
import           JSON.PrettyPrinter

parseFile :: FilePath -> IO ()
parseFile path = do
    contents <- readFile path
    case parse (parseJValue <* eof) "" contents of
        Left  e   -> red ("Error: " ++ show e)
        Right ast -> green (prettyJValue ast)


parseJValue :: Parser JValue
parseJValue = L.whiteSpace >> L.lexeme
    (   try parseFloat
    <|> parseNumber
    <|> parseObject
    <|> parseList
    <|> parseString
    )

parseObject :: Parser JValue
parseObject = JObject <$> M.fromList <$> L.braces (L.commaSep parsePair)
  where
    parsePair :: Parser (String, JValue)
    parsePair = do
        key <- L.stringLiteral <?> "key"
        L.colon
        value <- parseJValue <?> "value"
        pure (key, value)

parseList :: Parser JValue
parseList = JList <$> L.squareBrackets (L.commaSep parseJValue)


parseNumber :: Parser JValue
parseNumber = JInt <$> L.integer

parseString :: Parser JValue
parseString = JString <$> L.stringLiteral

parseFloat :: Parser JValue
parseFloat = JFloat <$> L.float
