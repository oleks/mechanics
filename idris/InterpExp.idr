module InterpExp

import Ast
import Data.Vect
import MonadExt

%access private
%default total

data State
  = MkState (List (String, FunBody len))

data InterpExp a
  = MkInterpExp (State -> (a, State))

bind' : InterpExp a -> (a -> InterpExp b) -> InterpExp b
bind' (MkInterpExp ma) f = MkInterpExp (\s =>
  let (a, s') = ma s
      (MkInterpExp mb) = f a
  in mb s')

pure' : a -> InterpExp a
pure' a = MkInterpExp (\s => (a, s))

Functor InterpExp where
  map f m = bind' m (\a => pure' (f a))

Applicative InterpExp where
  pure = pure'
  pf <*> px = bind' pf (\f => bind' px (pure . f))

Monad InterpExp where
  (>>=) = bind'

get : InterpExp State
get = MkInterpExp (\s => (s, s))

mutual
  public export
  add' : Int -> Expr -> Expr
  add' 0 e = e
  add' n e = ExpAdd (ExpVal n) e

  public export
  mul' : Int -> Expr -> Expr
  mul' 0 e = e
  mul' 1 e = e
  mul' n e = ExpMul (ExpVal n) e

  partial public export
  baseInterpExp : (Expr -> Expr -> Expr) -> (Int -> Int -> Int)
    -> (Int -> Expr -> Expr)
    -> Expr -> Expr -> Expr
  baseInterpExp op fn fc e1 e2 =
    let
      v1 = interpExp e1
      v2 = interpExp e2
    in
      case (v1, v2) of
        (ExpVal x, ExpVal y) => ExpVal $ fn x y
        (ExpVal c, e2) => fc c e2
        (e1, ExpVal c) => fc c e1
        _ => op v1 v2

  partial public export
  interpExp : Expr -> Expr
  interpExp (FnCall "fst" [(ExpTup e1 _)]) = e1
  interpExp (FnCall "snd" [(ExpTup _ e2)]) = e2
  interpExp (FnCall "diff" [e1, e2]) =
    interpExp $ diff e1 e2
  interpExp (ExpAdd e1 e2) = baseInterpExp ExpAdd (+) add' e1 e2
  interpExp (ExpMul e1 e2) = baseInterpExp ExpMul (*) mul' e1 e2
  interpExp v = v

  partial public export
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
