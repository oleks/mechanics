module Ast where

newtype Name
  = Name String
  deriving (Eq, Ord, Show)

newtype Dec
  = Dec (Name, [Name], Expr)
  deriving (Eq, Ord, Show)

data Expr
  = Call Name [Expr]
  | Num Double
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Ord, Show)

newtype Prog
  = Prog ([Dec], Expr)
  deriving (Eq, Ord, Show)

data ParserConfig
  = ParserConfig {
    curriedNotation :: Bool
  }
