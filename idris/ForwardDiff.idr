module ForwardDiff

import Ast

%access public export
%default total

partial
diff : Expr -> Expr -> Expr
diff (ExpNam n) (ExpNam m) =
  if n == m then ExpVal 1 else ExpVal 0
diff (ExpVal v) (ExpNam _) =
  ExpVal 0
