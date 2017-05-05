||| A simple parser combinator library
|||
||| Roughly based on
||| https://github.com/kfl/simpleparse/blob/master/SimpleParse.hs 
|||
||| MIT License
|||
||| Copyright (c) 2010-2017 Ken Friis Larsen
||| Copyright (c) 2016-2017 Oleks <oleks@oleks.info>
|||
||| Permission is hereby granted, free of charge, to any person obtaining a
||| copy of this software and associated documentation files (the "Software"),
||| to deal in the Software without restriction, including without limitation
||| the rights to use, copy, modify, merge, publish, distribute, sublicense,
||| and/or sell copies of the Software, and to permit persons to whom the
||| Software is furnished to do so, subject to the following conditions:
|||
||| The above copyright notice and this permission notice shall be included in
||| all copies or substantial portions of the Software.
|||
||| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
||| IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
||| FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
||| THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
||| LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
||| FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
||| DEALINGS IN THE SOFTWARE.

module SimpleParse

%access export
%default total

data Parser a = P (List Char -> (List (a, List Char)))

private
bind' : Parser a -> (a -> Parser b) -> Parser b
bind' (P ma) f = P ( \ s => do
  (a, s') <- ma s
  let (P mb) = f a
  mb s')

private
pure' : a -> Parser a
pure' a = P (\s => [(a, s)])

private
void : Monad m => m a -> m ()
void m = m >>= (\_ => pure ())

Functor Parser where
  map f m = bind' m (\a => pure' (f a))

Applicative Parser where
  pure = pure'
  pf <*> px = bind' pf (\f => bind' px (pure . f))

Monad Parser where
  (>>=) = bind'

get : Parser (List Char)
get = P (\ cs => [(cs, cs)])

getc : Parser Char
getc = P getc'
  where getc' : List Char -> (List (Char, List Char))
        getc' [] = []
        getc' (x::xs) = [(x, xs)]

reject : Parser a
reject = P (\ _ => [])

char : Char -> Parser Char
char c = do
  c' <- getc
  if c == c' then pure c else reject

string : String -> Parser String
string s = map pack (string' (unpack s))
  where string' : (List Char) -> Parser (List Char)
        string' [] = pure []
        string' (c::cs) = do
          char c
          string' cs
          pure (c::cs)

chars : List Char -> Parser Char
chars cs = do
  c <- getc
  if c `elem` cs then pure c else reject

wild : Parser ()
wild = void getc

eof : Parser ()
eof = do
  cs <- get
  case cs of
    []  => pure ()
    _   => reject

nonempty : Parser (List a) -> Parser (List a)
nonempty p = do
  as <- p
  case as of
    [] => reject
    _ => pure as

satisfy : (Char -> Bool) -> Parser Char
satisfy p = do
  c <- getc
  if p c then pure c else reject

parse : Parser a -> String -> (List (a, String))
parse (P p) s =
  let rs = p (unpack s)
      as = map fst rs
      ss = map pack (map snd rs)
  in zip as ss

fullParse : Parser a -> String -> List a
fullParse p s = map fst $ parse (p <* eof) s

Alternative Parser where
  empty = reject
  (P p) <|> (P q) = P $ \cs => p cs <|> q cs

infixl 2 <|>|

(<|>|) : Parser a -> Lazy (Parser a) -> Parser a
(<|>|) (P p) l = P $ \cs =>
  case p cs of
    [] => let (P q) = l in q cs
    x => x

infixl 2 <*>|

(<*>|) : Parser (a -> b) -> Lazy (Parser a) -> Parser b
(<*>|) (P p) l = P $ \cs => do
  (f, cs') <- p cs
  let (P q) = l
  (a, cs'') <- q cs'
  pure (f a, cs'')

mutual
  partial
  some : Parser a -> Parser (List a)
  some p = map (::) p <*>| many p

  partial
  many : Parser a -> Parser (List a)
  many p = some p <|> pure []

option : a -> Parser a -> Parser a
option v p = p <|> pure v

choice : List (Parser a) -> Parser a
choice [] = reject
choice (p::ps) = p <|> choice ps

between : Parser open -> Parser close
           -> Parser a -> Parser a
between open close p = (void $ open) *> p <* (void $ close)

partial
chainl1 : Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= chainl1'
  where
    chainl1' x = pure x <|> do
      f <- op
      y <- p
      chainl1' (f x y)

partial
chainr1 : Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= chainr1'
  where
    chainr1' x = pure x <|> do
      f <- op
      y <- chainr1 p op
      pure (f x y)

mutual
  partial
  sepBy : Parser a -> Parser sep -> Parser (List a)
  sepBy p sep = (p `sepBy1` sep) <|> pure []

  partial
  sepBy1 : Parser a -> Parser sep -> Parser (List a)
  sepBy1 p sep = do {a <- p; as <- many (sep *> p); pure (a::as)}

munch : (Char -> Bool) -> Parser (List Char)
munch p = do
    cs <- get
    scan cs
  where
    total
    scan : (List Char) -> Parser (List Char)
    scan (c::cs) =
      if (p c)
      then do
        _ <- getc
        cs' <- scan cs
        pure (c::cs')
      else pure []
    scan _ = pure []

munch1 : (Char -> Bool) -> Parser (List Char)
munch1 p = do
  cs <- munch p
  case cs of
    [] => reject
    _ => pure cs

spaces : Parser (List Char)
spaces = munch isSpace

spaces1 : Parser (List Char)
spaces1 = munch1 isSpace

token : Parser a -> Parser a
token = (*>) spaces

token1 : Parser a -> Parser a
token1 = (*>) spaces1

stoken : String -> Parser ()
stoken = void . token . string

stoken1 : String -> Parser ()
stoken1 = void . token1 . string

isolatedToken : Parser a -> Parser a
isolatedToken p = spaces1 *> p <* spaces1

isolatedSToken : String -> Parser ()
isolatedSToken = void . isolatedToken . string
