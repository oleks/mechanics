module Parser.Impl where

import Ast
import Control.Monad ( void )
import Text.ParserCombinators.Parsec hiding ( parse, Parser )
import Text.Parsec.Prim ( runP )

data ParamParsers
  = ParamParsers
  { formal :: Parser [Name]
  , actual :: Parser [Expr]
  }

data ParserState
  = ParserState {
    params :: ParamParsers
  }

type Parser = GenParser Char ParserState

keywords :: [String]
keywords = []

parseName :: Parser Name
parseName = do
  c <- oneOf ['a'..'z']
  cs <- many $ oneOf $ ['a'..'z'] ++ ['0'..'9']
  let name = c:cs
  if name `elem` keywords
  then unexpected $ name ++ ": it is a reserved keyword"
  else pure $ Name name

token' :: Parser a -> Parser a
token' = (*>) spaces

ctoken :: Char -> Parser ()
ctoken = void . token' . char

parseCurried :: Parser a -> Parser [a]
parseCurried = flip sepBy space

parens :: Parser a -> Parser a
parens = between (ctoken '(') (ctoken ')')

parseUncurried :: Parser a -> Parser [a]
parseUncurried = parens . flip sepBy (ctoken ',')

parseParams :: Parser [Name]
parseParams = do
  p <- fmap (formal . params) getState
  p

parseNumeral :: Parser Expr
parseNumeral = do
  c <- oneOf ['1'..'9']
  cs <- many $ oneOf ['0'..'9']
  let number = read $ c:cs
  pure $ Num number

parseExpr :: Parser Expr
parseExpr = parseNumeral

parse :: Parser a -> ParserState ->
  SourceName -> String -> Either ParseError a
parse p st = runP p st

curriedState = ParserState
  (ParamParsers
    (parseCurried parseName)
    (parseCurried parseExpr)
  )

uncurriedState = ParserState
  (ParamParsers
    (parseUncurried parseName)
    (parseUncurried parseExpr)
  )
