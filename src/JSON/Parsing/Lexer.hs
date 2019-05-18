module JSON.Parsing.Lexer
    ( lexeme
    , parens
    , commaSep
    , colon
    , braces
    , squareBrackets
    , whiteSpace
    , stringLiteral
    , integer
    , float
    )
where

import qualified Text.Parsec.Token             as T
import           Text.Parsec.String             ( Parser )
import           Text.Parsec.Char               ( spaces )
import           Text.Parsec.Language           ( emptyDef
                                                , LanguageDef
                                                )

lexer :: T.TokenParser ()
lexer = T.makeTokenParser language
  where
    language :: LanguageDef ()
    language = emptyDef { T.commentStart = "/*"
                        , T.commentEnd   = "*/"
                        , T.commentLine  = "//"
                        }

lexeme :: Parser a -> Parser a
lexeme = T.lexeme lexer

parens :: Parser a -> Parser a
parens = lexeme . T.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = lexeme . T.commaSep lexer

colon :: Parser String
colon = lexeme (T.colon lexer)

braces :: Parser a -> Parser a
braces = T.braces lexer

squareBrackets :: Parser a -> Parser a
squareBrackets = T.brackets lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer


stringLiteral :: Parser String
stringLiteral = T.stringLiteral lexer

integer :: Parser Integer
integer = T.integer lexer

float :: Parser Double
float = T.float lexer
