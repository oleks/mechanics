module Main

import Ast
import SimpleParse
import Parser.Impl

main : IO ()
main = do
  let r = parse (parseExpr1 <* eof) "1+2"
  printLn $ show $ r
