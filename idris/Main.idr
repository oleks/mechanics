module Main

import Ast
import Parser
import InterpExp

import Data.Vect
import Data.SortedMap

interpStr : String -> String
interpStr s =
  case fullParse parseExpr s of
    [] => "No parse"
    [r] => (show r) ++ "\n" ++ (show $ runInterpExp initialState r)
    rs => "Ambiguous grammar: " ++ show rs

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
main = repl' "> " interpStr
