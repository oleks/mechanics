module Main

import Ast
import SimpleParse
import Parser.Impl

showExpr : String -> IO ()
showExpr s = do
  let [(r, _)] = parse (parseExpr <* eof) s
  printLn $ show $ r

main : IO ()
main = do
  showExpr "1+2+3"
  showExpr "1*2*3"
  showExpr "1+2*3"
  showExpr "1*2+3"
