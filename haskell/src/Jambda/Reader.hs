module Jambda.Reader (
  jread,
) where

import qualified Data.Bifunctor as Bi
import Data.Void
import Flow
import Jambda.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void String

eatSpace :: Parser ()
eatSpace = Lexer.space space1 (Lexer.skipLineComment ";") empty

eatCommas :: Parser ()
eatCommas = skipMany $ char ','

eat :: Parser ()
eat = eatSpace <|> eatCommas

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme eat

symbol :: String -> Parser String
symbol = Lexer.symbol eat

betweenQuotes :: Parser a -> Parser a
betweenQuotes = between (symbol "\"") (symbol "\"")

betweenParens :: Parser a -> Parser a
betweenParens = between (symbol "(") (symbol ")")

nothing :: MonadParsec e s m => m ()
nothing = return ()

integer :: Parser Int
integer = Lexer.signed nothing (lexeme Lexer.decimal)

float :: Parser Double
float = Lexer.signed nothing (lexeme Lexer.float)

readInteger :: Parser JForm
readInteger = do
  x <- integer
  return $ JInteger x

readFloat :: Parser JForm
readFloat = do
  x <- float
  return $ JFloat x

readNum :: Parser JForm
readNum = try readFloat <|> readInteger

readList1 :: Parser JForm
readList1 = do
  forms <- betweenParens readForms
  return $ JList forms

readString :: Parser JForm
readString = do
  str <- betweenQuotes $ many $ noneOf "\\\""
  return $ JString str

readSymbol :: Parser JForm
readSymbol = do
  first <- letterChar <|> oneOf "+-"
  rest <- many alphaNumChar
  return $ case first:rest of
    "true" -> JBoolean True
    "false" -> JBoolean False
    "nil" -> JNothing
    x -> JIdentifier x

readQuote :: Parser JForm
readQuote = do
  _ <- char '\''
  form <- readForm
  return $ JList [JIdentifier "quote", form]

readForm :: Parser JForm
readForm = do
  eat
  form <-
    try readNum <|>
    readQuote <|>
    readList1 <|>
    readString <|>
    readSymbol
  eat
  return form

readForms :: Parser [JForm]
readForms = many readForm

readAll :: Parser JForm
readAll = do
  form <- readForm
  eof
  return form

jread :: String -> Either String JForm
jread input = (parse readAll "repl" input) |> Bi.first parseErrorPretty
