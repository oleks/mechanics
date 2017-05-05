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
  | ExpTup Expr Expr
  | ExpAdd Expr Expr
  | ExpSub Expr Expr
  | ExpMul Expr Expr
  | ExpDiv Expr Expr
  | ExpLet Name Expr Expr
  | ExpIf Expr Expr Expr

private
parens : String -> String
parens s = "( " ++ s ++ " )"

mutual
  -- The Idris totality checker is missing a couple cases, so here is some
  -- fairly verbose code to circumvent its limitations (for now).

  private
  showVect : Show a => Vect len a -> String
  showVect = concat . intersperse ", " . map show

  private
  showExpr : Expr -> String
  showExpr (ExpNam n) = n
  showExpr (ExpVal v) = show v
  showExpr (FnCall n v) = n ++ "( " ++ (showVect v) ++ " )"
  showExpr (ExpNeg e) = "~" ++ (parens $ showExpr e)
  showExpr (ExpTup e1 e2) = "< " ++ showExpr e1 ++ ", " ++ showExpr e2 ++ " >"
  showExpr (ExpAdd e1 e2) = parens $ showExpr e1 ++ " + " ++ showExpr e2
  showExpr (ExpSub e1 e2) = parens $ showExpr e1 ++ " - " ++ showExpr e2
  showExpr (ExpMul e1 e2) = parens $ showExpr e1 ++ " * " ++ showExpr e2
  showExpr (ExpDiv e1 e2) = parens $ showExpr e1 ++ " / " ++ showExpr e2
  showExpr (ExpLet n e1 e2) =
    "let " ++ n ++ " = " ++ (showExpr e1) ++ " in " ++ (showExpr e2)
  showExpr (ExpIf ec et ef) =
    "if " ++ (showExpr ec) ++
    " then " ++ (showExpr et) ++
    " else " ++ (showExpr ef)

  Show Expr where
    show = showExpr

mutual
  -- The Idris totality checker is missing a couple cases, so here is some
  -- fairly verbose code to circumvent its limitations (for now).

  private
  eqVect : Vect n Expr -> Vect m Expr -> Bool
  eqVect [] [] = True
  eqVect [] _ = False
  eqVect _ [] = False
  eqVect (v::vs) (w::ws) = eqExpr v w && eqVect vs ws

  private
  eqExpr : Expr -> Expr -> Bool
  eqExpr (ExpNam n) (ExpNam m) = n == m
  eqExpr (ExpVal v) (ExpVal w) = v == w
  eqExpr (FnCall n v) (FnCall m w) = n == m && eqVect v w
  eqExpr (ExpNeg e) (ExpNeg f) = e == f
  eqExpr (ExpTup e1 e2) (ExpTup f1 f2) = e1 == f1 && e2 == f2
  eqExpr (ExpAdd e1 e2) (ExpAdd f1 f2) = e1 == f1 && e2 == f2
  eqExpr (ExpSub e1 e2) (ExpSub f1 f2) = e1 == f1 && e2 == f2
  eqExpr (ExpMul e1 e2) (ExpMul f1 f2) = e1 == f1 && e2 == f2
  eqExpr (ExpDiv e1 e2) (ExpDiv f1 f2) = e1 == f1 && e2 == f2
  eqExpr (ExpLet n e1 e2) (ExpLet m f1 f2)
    = n == m && e1 == f1 && e2 == f2
  eqExpr (ExpIf ec et ef) (ExpIf fc ft ff)
    = ec == fc && et == ft && ef == ff
  eqExpr _ _ = False

  Eq Expr where
    (==) = eqExpr

data Clause : (len : Nat) -> Type where
  MkClause : Vect len Pattern -> Expr -> Clause len

FunBody : Nat -> Nat -> Type
FunBody m n = Vect m (Clause n)

data FunDec : (m : Nat) -> (n : Nat) -> Type where
  MkFunDec : Name -> FunBody m n -> FunDec m n
