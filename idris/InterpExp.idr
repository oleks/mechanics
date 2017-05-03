module InterpExp

import Ast
import Data.Vect
import MonadExt
import Data.SortedMap

%access public export
%default total

data State
  = MkState (SortedMap String (m ** n ** FunBody m n))

data InterpExp a
  = MkInterpExp (State -> a)

bind' : InterpExp a -> (a -> InterpExp b) -> InterpExp b
bind' (MkInterpExp ma) f = MkInterpExp (\s =>
  let a = ma s
      (MkInterpExp mb) = f a
  in mb s)

pure' : a -> InterpExp a
pure' a = MkInterpExp (\s => a)

Functor InterpExp where
  map f m = bind' m (\a => pure' (f a))

Applicative InterpExp where
  pure = pure'
  pf <*> px = bind' pf (\f => bind' px (pure . f))

Monad InterpExp where
  (>>=) = bind'

get : InterpExp State
get = MkInterpExp (\s => s)

getFun : String -> InterpExp (Maybe (m ** n ** FunBody m n))
getFun name = do
  (MkState st) <- get
  pure $ lookup name st

mutual
  add' : Int -> Expr -> Expr
  add' 0 e = e
  add' n e = ExpAdd (ExpVal n) e

  mul' : Int -> Expr -> Expr
  mul' 0 e = e
  mul' 1 e = e
  mul' n e = ExpMul (ExpVal n) e

  partial
  baseInterpExp : (Expr -> Expr -> Expr) -> (Int -> Int -> Int)
    -> (Int -> Expr -> Expr)
    -> Expr -> Expr -> InterpExp Expr
  baseInterpExp op fn fc e1 e2 = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    pure $ case (v1, v2) of
      (ExpVal x, ExpVal y) => ExpVal $ fn x y
      (ExpVal c, e2) => fc c e2
      (e1, ExpVal c) => fc c e1
      _ => op v1 v2

  partial
  interpExp : Expr -> InterpExp Expr
  interpExp (FnCall "fst" [(ExpTup e1 _)]) = pure e1
  interpExp (FnCall "snd" [(ExpTup _ e2)]) = pure e2
  interpExp (FnCall "diff" [e1, e2]) = do
    d <- diff e1 e2
    interpExp d
  interpExp (ExpAdd e1 e2) = baseInterpExp ExpAdd (+) add' e1 e2
  interpExp (ExpMul e1 e2) = baseInterpExp ExpMul (*) mul' e1 e2
  interpExp v = pure $ v

  partial
  diff : Expr -> Expr -> InterpExp Expr
  diff (ExpNam n) (ExpNam m) =
    pure $ if n == m then ExpVal 1 else ExpVal 0
  diff (ExpVal v) (ExpNam _) =
    pure $ ExpVal 0
  diff (ExpNeg e) ed = do
    d <- diff e ed
    interpExp $ ExpNeg d
  diff (ExpAdd e1 e2) ed = do
    d1 <- diff e1 ed
    d2 <- diff e2 ed
    interpExp $ ExpAdd d1 d2
  diff (ExpMul e1 e2) ed = do
    d1 <- diff e1 ed
    d2 <- diff e2 ed
    interpExp $ ExpAdd
      (ExpMul e1 d2)
      (ExpMul d1 e2)

interp : InterpExp a -> State -> a
interp (MkInterpExp i) s = i s

partial
runInterpExp : State -> Expr -> Expr
runInterpExp s e = interp (interpExp e) s
