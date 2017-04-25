module Interp

import Ast
import Data.Vect

%access public export
%default total

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
      v1 = interp e1
      v2 = interp e2
    in
      case (v1, v2) of
        (ExpVal x, ExpVal y) => ExpVal $ fn x y
        (ExpVal c, e2) => fc c e2
        (e1, ExpVal c) => fc c e1
        _ => op v1 v2

  partial
  interp : Expr -> Expr
  interp (FnCall "fst" [(ExpTup e1 _)]) = e1
  interp (FnCall "snd" [(ExpTup _ e2)]) = e2
  interp (FnCall "diff" [e1, e2]) =
    interp $ diff e1 e2
  interp (ExpAdd e1 e2) = baseInterp ExpAdd (+) add' e1 e2
  interp (ExpMul e1 e2) = baseInterp ExpMul (*) mul' e1 e2
  interp v = v

  partial
  diff : Expr -> Expr -> Expr
  diff (ExpNam n) (ExpNam m) =
    if n == m then ExpVal 1 else ExpVal 0
  diff (ExpVal v) (ExpNam _) =
    ExpVal 0
  diff (ExpNeg e) ed =
    interp $ ExpNeg (diff e ed)
  diff (ExpAdd e1 e2) ed =
    interp $ ExpAdd (diff e1 ed) (diff e2 ed)
  diff (ExpMul e1 e2) ed =
    interp $ ExpAdd
      (ExpMul e1 (diff e2 ed))
      (ExpMul (diff e1 ed) e2)
