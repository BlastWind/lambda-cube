module Untyped.ReadPrint (parseTerm, parseTermTest, prettyPrint) where

import Text.Megaparsec (Parsec, between, choice, empty, eof, many, oneOf, parse, parseTest, some, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)
import Control.Monad (void)
import Untyped.Types

type Parser = Parsec Void String


-- Lexer space consumer
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Basic tokens
nameFirst :: Parser Char
nameFirst = letterChar <|> oneOf ("_" :: String)

nameRest :: Parser Char
nameRest = alphaNumChar <|> oneOf ("_'" :: String)

-- Parser for names (variables)
name :: Parser String
name = lexeme $ do
  first <- nameFirst
  rest <- many nameRest
  let result = first : rest
  -- Ensure the name isn't a reserved word
  return result

-- Parser for simple terms
simpleTerm :: Parser Term
simpleTerm =
  Var
    <$> name
      <|> between (symbol "(") (symbol ")") expr

-- Parser for application terms (left-associative)
appTerm :: Parser Term
appTerm = foldl App <$> simpleTerm <*> many simpleTerm

-- Parser for lambda abstractions
lambdaTerm :: Parser Term
lambdaTerm = do
  -- The lambda symbol may be either 'λ' or '\'
  void $
    choice
      [ symbol "λ",
        symbol "\\"
      ]
  var <- name
  void $ symbol "."
  Lam var <$> expr

-- Main expression parser
expr :: Parser Term
expr = try lambdaTerm <|> appTerm

-- Full parser with whitespace handling
parseTerm :: String -> Either (ParseErrorBundle String Void) Term
parseTerm = parse (sc *> expr <* eof) "<input>"

parseTermTest :: String -> IO ()
parseTermTest = parseTest (sc *> expr <* eof)

prettyPrint :: Term -> String
prettyPrint (Var x) = x
prettyPrint (App p q) = "(" ++ prettyPrint p ++ prettyPrint q ++ ")"
prettyPrint (Lam v p) = "(λ" ++ v ++ "." ++ " " ++ prettyPrint p ++ ")"