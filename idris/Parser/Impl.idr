module Parser.Impl

import Ast
import SimpleParse
import Data.String

%access export

parseVal : Parser Expr
parseVal = parseZero <|> parseNonZero
  where
    parseZero : Parser Expr
    parseZero = char '0' *> pure (ExpVal 0)

    step : (Int, Int) -> Char -> (Int, Int)
    step (acc, scale) c = (acc + (ord c - ord '0') * scale, scale * 10)

    parseNonZero : Parser Expr
    parseNonZero = do
      c <- chars ['1'..'9']
      cs <- munch $ flip elem ['0'..'9']
      if length cs > 4 then reject else pure ()
      let (i, _) = foldl step (0, 1) $ reverse (c::cs)
      pure $ ExpVal i

parseExpr0 : Parser Expr
parseExpr0 = parseVal

parseExpr1 : Parser Expr
parseExpr1 = chainl1 parseExpr0 (char '+' *> pure ExpAdd)
