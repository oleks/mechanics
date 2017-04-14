||| Roughly based on
||| https://github.com/kfl/simpleparse/blob/master/SimpleParse.hs 
||| See also https://github.com/oleks/Idris-SimpleParse

module SimpleParse

%access export

data Parser a = P (List Char -> (List (a, List Char)))

total private
bind' : Parser a -> (a -> Parser b) -> Parser b
bind' (P ma) f = P ( \ s => do
  (a, s') <- ma s
  let (P mb) = f a
  mb s')

total private
pure' : a -> Parser a
pure' a = P (\s => [(a, s)])

total private
void : Monad m => m a -> m ()
void m = m >>= (\_ => pure ())

Functor Parser where
  map f m = bind' m (\a => pure' (f a))

Applicative Parser where
  pure = pure'
  pf <*> px = bind' pf (\f => bind' px (pure . f))

Monad Parser where
  (>>=) = bind'

total
get : Parser (List Char)
get = P (\ cs => [(cs, cs)])

total
getc : Parser Char
getc = P getc'
  where getc' : List Char -> (List (Char, List Char))
        getc' [] = []
        getc' (x::xs) = [(x, xs)]

total
reject : Parser a
reject = P (\ _ => [])

total
char : Char -> Parser Char
char c = do
  c' <- getc
  if c == c' then pure c else reject

total
string : String -> Parser String
string s = map pack (string' (unpack s))
  where string' : (List Char) -> Parser (List Char)
        string' [] = pure []
        string' (c::cs) = do
          char c
          string' cs
          pure (c::cs)

total
chars : List Char -> Parser Char
chars cs = do
  c <- getc
  if c `elem` cs then pure c else reject

total
wild : Parser ()
wild = void getc

total
eof : Parser ()
eof = do
  cs <- get
  case cs of
    []  => pure ()
    _   => reject

total
nonempty : Parser (List a) -> Parser (List a)
nonempty p = do
  as <- p
  case as of
    [] => reject
    _ => pure as

total
satisfy : (Char -> Bool) -> Parser Char
satisfy p = do
  c <- getc
  if p c then pure c else reject

total
parse : Parser a -> String -> (List (a, String))
parse (P p) s =
  let rs = p (unpack s)
      as = map fst rs
      ss = map pack (map snd rs)
  in zip as ss

total
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
  some : Alternative f => f a -> f (List a)
  some p = [| p :: many p |]

  many : Alternative f => f a -> f (List a)
  many p = some p <|> pure []

many' : Parser a -> Parser (List a)
many' p = ((pure (::) <*> p) <*>| many' p) <|> pure []

total
option : a -> Parser a -> Parser a
option v p = p <|> pure v

total
choice : List (Parser a) -> Parser a
choice [] = reject
choice (p::ps) = p <|> choice ps

total
between : Parser open -> Parser close
           -> Parser a -> Parser a
between open close p = (void $ open) *> p <* (void $ close)

chainl1 : Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= chainl1'
  where
    chainl1' x = pure x <|> do
      f <- op
      y <- p
      chainl1' (f x y)

chainr1 : Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= chainr1'
  where
    chainr1' x = pure x <|> do
      f <- op
      y <- chainr1 p op
      pure (f x y)

mutual
  sepBy : Parser a -> Parser sep -> Parser (List a)
  sepBy p sep = (p `sepBy1` sep) <|> pure []

  sepBy1 : Parser a -> Parser sep -> Parser (List a)
  sepBy1 p sep = do {a <- p; as <- many (sep *> p); pure (a::as)}

total
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

total
munch1 : (Char -> Bool) -> Parser (List Char)
munch1 p = do
  cs <- munch p
  case cs of
    [] => reject
    _ => pure cs

total
spaces : Parser (List Char)
spaces = munch isSpace

total
token : Parser a -> Parser a
token p = spaces *> p

total
stoken : String -> Parser ()
stoken = void . token . string

total
ctoken : Char -> Parser ()
ctoken = void . token . char
