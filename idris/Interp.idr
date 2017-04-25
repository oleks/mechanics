module Interp

import Ast
import Data.Vect
import MonadExt

%access public export
%default total

data State
  = MkState (List (String, Clause len))

data Interp a
  = MkInterp (State -> (a, State))

private
bind' : Interp a -> (a -> Interp b) -> Interp b
bind' (MkInterp ma) f = MkInterp (\s =>
  let (a, s') = ma s
      (MkInterp mb) = f a
  in mb s')

private
pure' : a -> Interp a
pure' a = MkInterp (\s => (a, s))


Functor Interp where
  map f m = bind' m (\a => pure' (f a))

Applicative Interp where
  pure = pure'
  pf <*> px = bind' pf (\f => bind' px (pure . f))

Monad Interp where
  (>>=) = bind'

get : Interp State
get = MkInterp (\s => (s, s))

mutual
  add' : Int -> Expr -> Expr
  add' 0 e = e
  add' n e = ExpAdd (ExpVal n) e

  mul' : Int -> Expr -> Expr
  mul' 0 e = e
  mul' 1 e = e
  mul' n e = ExpMul (ExpVal n) e

  partial
  baseInterp : (Expr -> Expr -> Expr) -> (Int -> Int -> Int)
    -> (Int -> Expr -> Expr)
    -> Expr -> Expr -> Expr
  baseInterp op fn fc e1 e2 =
    let
      v1 = interpExp e1
      v2 = interpExp e2
    in
      case (v1, v2) of
        (ExpVal x, ExpVal y) => ExpVal $ fn x y
        (ExpVal c, e2) => fc c e2
        (e1, ExpVal c) => fc c e1
        _ => op v1 v2

  partial
  interpExp : Expr -> Expr
  interpExp (FnCall "fst" [(ExpTup e1 _)]) = e1
  interpExp (FnCall "snd" [(ExpTup _ e2)]) = e2
  interpExp (FnCall "diff" [e1, e2]) =
    interpExp $ diff e1 e2
  interpExp (ExpAdd e1 e2) = baseInterp ExpAdd (+) add' e1 e2
  interpExp (ExpMul e1 e2) = baseInterp ExpMul (*) mul' e1 e2
  interpExp v = v

  partial
  diff : Expr -> Expr -> Expr
  diff (ExpNam n) (ExpNam m) =
    if n == m then ExpVal 1 else ExpVal 0
  diff (ExpVal v) (ExpNam _) =
    ExpVal 0
  diff (ExpNeg e) ed =
    interpExp $ ExpNeg (diff e ed)
  diff (ExpAdd e1 e2) ed =
    interpExp $ ExpAdd (diff e1 ed) (diff e2 ed)
  diff (ExpMul e1 e2) ed =
    interpExp $ ExpAdd
      (ExpMul e1 (diff e2 ed))
      (ExpMul (diff e1 ed) e2)
