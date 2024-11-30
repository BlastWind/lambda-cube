{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}

module SimplyTyped.ReadPrint (parseTerm, pPrint, render) where

import Control.Monad (replicateM_, void)
import Data.Tree
import Data.Void (Void)
import SimplyTyped.Types
import Text.Megaparsec (Parsec, between, choice, empty, eof, many, oneOf, parse, parseTest, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.PrettyPrint hiding (empty, (<>))
import Text.PrettyPrint.HughesPJClass (Pretty (..))

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

varTerm :: Parser Term
varTerm = Var <$> name

-- Parser for simple terms
simpleTerm :: Parser Term
simpleTerm =
  varTerm <|> between (symbol "(") (symbol ")") expr

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
  void $ symbol ":"
  typ <- typeTerm
  void $ symbol "."
  Lam var typ <$> expr

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan = do x <- p; rest x
    rest x =
      do
        f <- op
        f x <$> scan
        <|> return x

typeTerm :: Parser Type
typeTerm =
  chainr1
    (TypeVar <$> name <|> between (symbol "(") (symbol ")") typeTerm)
    (symbol "->" *> return FunctionType)

-- Main expression parser
expr :: Parser Term
expr = try lambdaTerm <|> try appTerm <|> varTerm

-- Full lambda parser with whitespace handling
parseTerm :: String -> Either (ParseErrorBundle String Void) Term
parseTerm = parse (sc *> expr <* eof) "<input>"

parseTree :: Parser a -> Parser (Tree a)
parseTree p = go 0 p
  where
    go identLevel p = do
      replicateM_ identLevel (symbol " ")
      root <- p
      children <- many (go (identLevel + 1) p)
      return $ Node root children

prettyTerm :: Term -> Doc
prettyTerm = prettyPrec 0
  where
    prettyPrec :: Int -> Term -> Doc
    prettyPrec p term = case term of
      Var v -> text v
      App t1 t2 ->
        maybeParens (p > 0) $
          prettyPrec 1 t1 <+> prettyPrec 2 t2
      Lam v typ term ->
        maybeParens (p > 0) $
          char 'λ' <> (text v <> (char '.' <+> prettyPrec 0 term))

    maybeParens :: Bool -> Doc -> Doc
    maybeParens True = parens
    maybeParens False = id

instance Pretty Term where
  pPrint = prettyTerm

instance (Pretty a) => Pretty (Tree a) where
  pPrint = go 0
    where
      go level (Node root children) =
        text (replicate level ' ') <> pPrint root <> char '\n' <> hcat (map (go (level + 1)) children)