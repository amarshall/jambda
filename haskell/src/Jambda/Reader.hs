module Jambda.Reader (
  jread,
) where

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

integer :: Parser Integer
integer = lexeme Lexer.decimal

readInteger :: Parser JForm
readInteger = do
  x <- integer
  return $ JInteger x

readList1 :: Parser JForm
readList1 = do
  forms <- betweenParens $ sepEndBy readForm eat
  return $ JList forms

readString :: Parser JForm
readString = do
  str <- betweenQuotes $ many $ noneOf "\\\""
  return $ JString str

readSymbol :: Parser JForm
readSymbol = do
  first <- letterChar
  rest <- many alphaNumChar
  return $ case first:rest of
    "true" -> JBoolean True
    "false" -> JBoolean False
    "nil" -> JNothing
    x -> JIdentifier x

readForm :: Parser JForm
readForm = do
  eat
  form <-
    readInteger <|>
    readList1 <|>
    readString <|>
    readSymbol
  return $ form

jread :: String -> Maybe JForm
jread input = (parse readForm "repl" input) |> either (const Nothing) Just
