module Parser

import SimpleParse
import public Parser.Impl

public export
Parser : Type -> Type
Parser = SimpleParse.Parser

export
parse : Parser.Parser a -> String -> (List (a, String))
parse = SimpleParse.parse

export
fullParse : Parser.Parser a -> String -> List a
fullParse = SimpleParse.fullParse
