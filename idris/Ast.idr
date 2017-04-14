module Ast

import Data.Vect

Name : Type
Name = String

Value : Type
Value = Double

data Pattern
  = PatNam Name
  | PatVal Value

data Expr
  = ExpNam Name
  | ExpVal Value
  | ExpNeg Expr
  | ExpAdd Expr Expr
  | ExpSub Expr Expr
  | ExpMul Expr Expr
  | ExpDiv Expr Expr

data Clause : (len : Nat) -> Type where
  MkClause : Vect len Pattern -> Expr -> Clause len

data FunDec : (len : Nat) -> Type where
  MkFunDec : Name -> List (Clause len) -> FunDec len
