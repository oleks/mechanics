module Interp

import Ast
import Data.Vect
import ForwardDiff

%access public export
%default total

partial
interp : Expr -> Expr
interp (FnCall "diff" [e1, e2]) = diff e1 e2
interp (ExpAdd e1 e2) =
  let
    v1 = interp e1
    v2 = interp e2
  in
    case (v1, v2) of
      (ExpVal x, ExpVal y) => ExpVal $ x + y
      _ => ExpAdd v1 v2
interp v = v
