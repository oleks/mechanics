module Ast

import Data.Vect

%access public export
%default total

Name : Type
Name = String

Value : Type
Value = Double

data Pattern
  = PatNam Name
  | PatVal Value

data Expr
  = ExpNam Name
  | ExpVal Int
  | FnCall Name (Vect len Expr)
  | ExpNeg Expr
  | ExpAdd Expr Expr
  | ExpSub Expr Expr
  | ExpMul Expr Expr
  | ExpDiv Expr Expr

private
parens : String -> String
parens s = "( " ++ s ++ " )"

Show Expr where
  show (ExpNam n) = show n
  show (ExpVal v) = show v
  show (FnCall n v) = show n ++ " " ++ show v
  show (ExpNeg e) = "-( " ++ show e ++ " )"
  show (ExpAdd e1 e2) = parens $ show e1 ++ " + " ++ show e2
  show (ExpSub e1 e2) = parens $ show e1 ++ " - " ++ show e2
  show (ExpMul e1 e2) = parens $ show e1 ++ " * " ++ show e2
  show (ExpDiv e1 e2) = parens $ show e1 ++ " / " ++ show e2

data Clause : (len : Nat) -> Type where
  MkClause : Vect len Pattern -> Expr -> Clause len

data FunDec : (len : Nat) -> Type where
  MkFunDec : Name -> List (Clause len) -> FunDec len
