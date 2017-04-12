module Parser
  ( ParseError
  , parseString
  , parseFile
  ) where

import Parser.Impl
  ( parseString
  , parseFile
  )

import Text.ParserCombinators.Parsec ( ParseError )
