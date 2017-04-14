module SimpleParse (
  Parser,
  reject,
  char, string, chars, wild, eof,
  parse, fullParse,
  empty, (<|>), many, some,
  nonempty,
  satisfy, option, choice, between,
  chainl1, chainr1,
  sepBy, sepBy1,
  munch, munch1,
  spaces, token, stoken
) where

import Control.Applicative
  ( Alternative((<|>), empty, many, some) )

import Control.Monad ( void )
import Data.Char ( isSpace )

newtype Parser a = Parser {
  runParser :: String -> [(a, String)]
}

instance Functor Parser where
  fmap f m = m >>= \a -> return (f a)

instance Applicative Parser where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

instance Monad Parser where
  -- return :: a -> Parser a
  return a = Parser $ \ s -> [(a, s)]

  -- (>>=) :: Parser a
  --          -> (a -> Parser b)
  --          -> Parser b 
  m >>= f = Parser $ \s -> do
    (a, s') <- runParser m s
    runParser (f a) s'

  -- fail :: String -> Parser a
  fail _ = Parser $ \ _ -> []

get :: Parser String
get = Parser $ \ s -> [(s, s)]

getc :: Parser Char
getc = Parser getc'
  where getc' "" = []
        getc' (x:xs) = [(x, xs)]

reject :: Parser a
reject = Parser $ \ _ -> []

char :: Char -> Parser Char
char c = do
  c' <- getc
  if c == c' then return c else reject

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  void $ char c
  void $ string cs
  return (c:cs)

chars :: [Char] -> Parser Char
chars cs = do
  c <- getc
  if c `elem` cs then return c else reject

wild :: Parser ()
wild = void $ getc

eof :: Parser ()
eof = do
  s <- get
  case s of
    ""  -> return ()
    _   -> reject

parse :: Parser a -> String -> [(a, String)]
parse = runParser

fullParse :: Parser a -> String -> [a]
fullParse p s = map fst $ runParser (p <* eof) s

instance Alternative Parser where
  -- empty :: a -> Parser a
  empty = reject

  -- (<|>) :: Parser a -> Parser a
  --          -> Parser a
  p <|> q = Parser $ \cs ->
    parse p cs <|> parse q cs

nonempty :: Parser [a] -> Parser [a]
nonempty p = do
  as <- p
  case as of
    [] -> reject
    _ -> return as

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- getc
  if p c then return c else reject

option :: a -> Parser a -> Parser a
option v p = p <|> return v

choice :: [Parser a] -> Parser a
choice [] = reject
choice (p:ps) = p <|> choice ps

between :: Parser open -> Parser close
           -> Parser a -> Parser a
between open close p = (void $ open) >> p <* (void $ close)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= chainl1'
  where
    chainl1' x = return x <|> do
      f <- op
      y <- p
      chainl1' (f x y)

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= chainr1'
  where
    chainr1' x = return x <|> do
      f <- op
      y <- chainr1 p op
      return (f x y)

sepBy           :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep    = (p `sepBy1` sep) <|> return []

sepBy1          :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep   = do {a <- p; as <- many (do {_ <- sep; p}); return (a:as)}

munch :: (Char -> Bool) -> Parser String
munch p = do
  s <- get
  scan s
  where
    scan (c:cs) | p c = do
      _ <- getc
      s <- scan cs
      return $ c:s
    scan _ = return ""

munch1 :: (Char -> Bool) -> Parser String
munch1 p = nonempty $ munch p

spaces :: Parser String
spaces = munch isSpace

token :: Parser a -> Parser a
token p = spaces >> p

stoken :: String -> Parser ()
stoken = void . token . string
