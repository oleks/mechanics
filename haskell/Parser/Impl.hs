module Parser.Impl where

import Ast
import Data.Char ( ord )
import Control.Monad ( void )
import Text.ParserCombinators.Parsec hiding ( parse, Parser )
import qualified Text.ParserCombinators.Parsec as P

type Parser = GenParser Char ()

keywords :: [String]
keywords = ["let", "in", "if", "then", "else"]

ptoken :: Parser a -> Parser a
ptoken = flip (<*) spaces

ctoken :: Char -> Parser ()
ctoken c = void $ ptoken $ char c

pValue :: Parser Value
pValue = ptoken (pZero <|> pNonZero)
  where
    pZero :: Parser Value
    pZero = char '0' *> pure 0.0

    step :: (Double, Double) -> Char -> (Double, Double)
    step (acc, scale) c =
      (acc + fromIntegral (ord c - ord '0') * scale, scale * 10.0)

    pNonZero :: Parser Value
    pNonZero = do
      c <- oneOf ['1'..'9']
      cs <- many $ oneOf ['0'..'9']
      let (n, _) = foldl step (0.0, 1.0) (c:cs)
      pure n

pName :: Parser Name
pName = ptoken (do
  c <- oneOf ['a'..'z']
  cs <- many $ oneOf $ ['a'..'z'] ++ ['0'..'9']
  let name = c:cs
  if name `elem` keywords
  then unexpected $ name ++ ": it is a reserved keyword"
  else pure $ name) <?> "name"

pExpr0 :: Parser Expr
pExpr0 = choice [
    fmap ExpNam pName,
    fmap ExpVal pValue
  ]

pExpr1 :: Parser Expr
pExpr1 = chainl1 pExpr0 op
  where
    op :: Parser (Expr -> Expr -> Expr)
    op = choice [
        ctoken '*' >> return ExpMul,
        ctoken '/' >> return ExpDiv
      ]

pExpr2 :: Parser Expr
pExpr2 = chainl1 pExpr1 op
  where
    op :: Parser (Expr -> Expr -> Expr)
    op = choice [
        ctoken '+' >> return ExpAdd,
        ctoken '-' >> return ExpSub
      ]

pExpr :: Parser Expr
pExpr = pExpr2

parse :: Parser a -> String -> Either ParseError a
parse p s = P.parse p "" s

fullParse :: Parser a -> String -> Either ParseError a
fullParse p s = P.parse (spaces >> p <* (ptoken eof)) "" s

parseString :: String -> Either ParseError Expr
parseString = fullParse pExpr

parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile path = fmap parseString (readFile path)
