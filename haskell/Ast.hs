module Ast (
  Name,
  Value,
  Pattern(..),
  Expr(..)
) where

import Text.PrettyPrint.HughesPJClass ( Pretty, pPrint )
import Text.PrettyPrint

type Name = String

type Value = Double

data Pattern
  = PatName Name
  | PatVal Value
  deriving (Show)

data Expr
  = ExpNam Name
  | ExpVal Value
  | FnCall Name [Expr]
  | ExpNeg Expr
  | ExpTup [Expr]
  | ExpAdd Expr Expr
  | ExpSub Expr Expr
  | ExpMul Expr Expr
  | ExpDiv Expr Expr
  | ExpLet Name Expr Expr
  | ExpIf Expr Expr Expr
  deriving (Show)

binop :: Expr -> String -> Expr -> Doc
binop e1 op e2 = pPrint e1 <+> text op <+> pPrint e2

instance Pretty Expr where
  pPrint (ExpNam n) = text n
  pPrint (ExpVal v) = double v
  pPrint (FnCall n v) = text n <+> (hsep $ map pPrint v)
  pPrint (ExpNeg e) = char '~' <> pPrint e
  pPrint (ExpTup es) = hsep $ punctuate (text ",") (map pPrint es)
  pPrint (ExpAdd e1 e2) = binop e1 "+" e2
  pPrint (ExpSub e1 e2) = binop e1 "-" e2
  pPrint (ExpMul e1 e2) = binop e1 "*" e2
  pPrint (ExpDiv e1 e2) = binop e1 "/" e2
  pPrint (ExpLet n e1 e2) =
    text "let" <+> text n <+> text "=" <+>
      pPrint e1 <+> text "in" <+> pPrint e2
  pPrint (ExpIf ec et ef) =
    text "if" <+> pPrint ec <+>
      text "then" <+> pPrint et <+>
      text "else" <+> pPrint ef

instance Eq Expr where
  (==) (ExpNam n) (ExpNam m) = n == m
  (==) (ExpVal v) (ExpVal w) = v == w
  (==) (FnCall n v) (FnCall m w) = n == m && v == w
  (==) (ExpNeg e) (ExpNeg f) = e == f
  (==) (ExpTup es) (ExpTup fs) = es == fs
  (==) (ExpAdd e1 e2) (ExpAdd f1 f2) = e1 == f1 && e2 == f2
  (==) (ExpSub e1 e2) (ExpSub f1 f2) = e1 == f1 && e2 == f2
  (==) (ExpMul e1 e2) (ExpMul f1 f2) = e1 == f1 && e2 == f2
  (==) (ExpDiv e1 e2) (ExpDiv f1 f2) = e1 == f1 && e2 == f2
  (==) (ExpLet n e1 e2) (ExpLet m f1 f2)
    = n == m && e1 == f1 && e2 == f2
  (==) (ExpIf ec et ef) (ExpIf fc ft ff)
    = ec == fc && et == ft && ef == ff
  (==) _ _ = False
