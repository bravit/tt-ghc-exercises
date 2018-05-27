module Main where

import Control.Monad
import Test.HUnit

import ArithTests
import qualified FullSimpleTests as F
import Syntax
import TestUtils
import Evaluator
import TaplError
import Parser

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullSimple) F.parseTests
                        , map (makeEvalTest  parseAndEval)    F.evalTests
                        , map (makeEvalTest  parseAndEval)    tyarithEvalTests
                        , [testDotFTest]
                        ]
                         

main :: IO ()
main = getAllTests >>= runTests
