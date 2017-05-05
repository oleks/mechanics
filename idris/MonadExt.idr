module MonadExt

%access public export
%default total

void : Monad m => m a -> m ()
void m = m >>= (\_ => pure ())


-- From Haskell's Control.Monad; base-4.9.1.0
-- (c) The University of Glasgow 2001

infixr 1 <=<, >=>

(>=>) : Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x => f x >>= g

(<=<) : Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)
