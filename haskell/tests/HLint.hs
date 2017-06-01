module HLint ( tests ) where

import Language.Haskell.HLint ( hlint )
import Distribution.TestSuite

arguments :: [String]
arguments =
    [ "src"
    , "tests"
    ]

tests :: IO [Test]
tests = do
  hints <- hlint arguments
  return $ if null hints then [ Test succeeds ] else [ Test fails ]
  where
    succeeds = TestInstance
        { run = return $ Finished Pass
        , name = "succeeds"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right succeeds
        }
    fails = TestInstance
        { run = return $ Finished $ Fail "Always fails!"
        , name = "fails"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right fails
        }
