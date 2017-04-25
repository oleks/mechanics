module MonadExt

%access public export
%default total

void : Monad m => m a -> m ()
void m = m >>= (\_ => pure ())
