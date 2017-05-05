module Parser.Impl

import Ast
import SimpleParse
import Data.String
import Data.Vect

%access export

parseVal : Parser Expr
parseVal = token (parseZero <|> parseNonZero)
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
parseNam = token $ do
  c <- chars ['a'..'z']
  cs <- munch $ flip elem $ ['a'..'z'] ++ ['0'..'9']
  pure $ pack $ c :: cs

parens : Parser a -> Parser a
parens = between (stoken "(") (stoken ")")

parseUncurried : Parser a -> Parser (List a)
parseUncurried = parens . flip sepBy1 (stoken ",")

mutual
  parseExpr0 : Parser Expr
  parseExpr0 = token (
    parseVal <|>| parseVarCall <|>|
    parseTuple <|>| parens parseExpr)

  parseExpr1 : Parser Expr
  parseExpr1 = chainl1 parseExpr0 $ choice
    [ stoken "*" *> pure ExpMul
    , stoken "/" *> pure ExpDiv
    ]

  parseExpr2 : Parser Expr
  parseExpr2 = chainl1 parseExpr1 $ choice
    [ stoken "+" *> pure ExpAdd
    , stoken "-" *> pure ExpSub
    ]

  parseArgs : Parser (List Expr)
  parseArgs = parseUncurried parseExpr

  parseExpr : Parser Expr
  parseExpr = parseLet <|>| parseExpr2

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
  parseTuple = between (stoken "<") (stoken ">") $ do
    e1 <- parseExpr
    stoken ","
    e2 <- parseExpr
    pure $ ExpTup e1 e2

  parseLet : Parser Expr
  parseLet = do
    stoken "let"
    spaces1
    name <- parseNam
    stoken "="
    letexpr <- parseExpr
    isolatedSToken "in"
    inexpr <- parseExpr
    pure $ ExpLet name letexpr inexpr
