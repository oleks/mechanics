module Test.Parser

import Ast
import Parser

assert : String -> Bool -> IO()
assert name b = do
  putStr name
  if b then putStrLn ": Passed"
  else putStrLn ": Failed"

assertEq : Eq a => String -> (actual : a) -> (expected : a) -> IO ()
assertEq name g e = assert name (g == e)

assertNotEq : Eq a => String -> (actual : a) -> (expected : a) -> IO ()
assertNotEq name g e = assert name (not(g == e))

showExpr : String -> List Expr
showExpr = fullParse parseExpr

export
testAddAssoc : IO ()
testAddAssoc = assertEq "testAddAssoc"
  (showExpr "1+2+3")
  [ExpAdd (ExpAdd (ExpVal 1) (ExpVal 2)) (ExpVal 3)]

export
testSubAssoc : IO ()
testSubAssoc = assertEq "testSubAssoc"
  (showExpr "1-2-3")
  [ExpSub (ExpSub (ExpVal 1) (ExpVal 2)) (ExpVal 3)]

export
testMulAssoc : IO ()
testMulAssoc = assertEq "testMulAssoc"
  (showExpr "1*2*3")
  [ExpMul (ExpMul (ExpVal 1) (ExpVal 2)) (ExpVal 3)]

export
testDivAssoc : IO ()
testDivAssoc = assertEq "testSubAssoc"
  (showExpr "1/2/3")
  [ExpDiv (ExpDiv (ExpVal 1) (ExpVal 2)) (ExpVal 3)]
