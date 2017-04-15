module Main

import Ast
import Parser

showExpr : String -> IO ()
showExpr s = do
  let [r] = fullParse parseExpr s
  printLn $ show $ r

main : IO ()
main = do
  showExpr "1+2+3"
  showExpr "1*2*3"
  showExpr "1+2*3"
  showExpr "1*2+3"
