module Main

import Ast
import Parser

showExpr : String -> String
showExpr s = let [r] = fullParse parseExpr s in show r

repl' : String -> (String -> String) -> IO ()
repl' prompt fn = do
  putStrLn prompt
  x <- getLine
  case unpack x of
    [] => pure ()
    _ => do
      putStrLn $ fn x
      repl' prompt fn

main : IO ()
main = repl' "> " showExpr
