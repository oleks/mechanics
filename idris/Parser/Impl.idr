module Parser.Impl

import Ast
import SimpleParse
import Data.String
import Data.Vect

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

parseNam : Parser Name
parseNam = do
  c <- chars ['a'..'z']
  cs <- munch $ flip elem $ ['a'..'z'] ++ ['0'..'9']
  pure $ pack $ c :: cs

parens : Parser a -> Parser a
parens = between (char '(') (char ')')

parseUncurried : Parser a -> Parser (List a)
parseUncurried = parens . flip sepBy1 (char ',')

mutual
  parseExpr0 : Parser Expr
  parseExpr0 = parseVal <|>|
                parseVarCall <|>|
                parseTuple <|>|
                parens parseExpr

  parseExpr1 : Parser Expr
  parseExpr1 = chainl1 parseExpr0 $ choice
    [ char '*' *> pure ExpMul
    , char '/' *> pure ExpDiv
    ]

  parseExpr2 : Parser Expr
  parseExpr2 = chainl1 parseExpr1 $ choice
    [ char '+' *> pure ExpAdd
    , char '-' *> pure ExpSub
    ]

  parseArgs : Parser (List Expr)
  parseArgs = parseUncurried parseExpr

  parseExpr : Parser Expr
  parseExpr = parseExpr2

  parseCall : Name -> Parser Expr
  parseCall name = do
    args <- parseArgs
    let vect = fromList args
    pure $ FnCall name vect

  parseVarCall : Parser Expr
  parseVarCall = do
    name <- parseNam
    parseCall name <|>| (pure $ ExpNam name)

  parseTuple : Parser Expr
  parseTuple = between (char '<') (char '>') $ do
    e1 <- parseExpr
    char ','
    e2 <- parseExpr
    pure $ ExpTup e1 e2

