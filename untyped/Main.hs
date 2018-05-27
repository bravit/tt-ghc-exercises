module Main where

import TaplError
import Util
import Evaluator

runParseAndEval = putStrLn . runThrows . parseAndEval

main :: IO ()
main = getContentsFromCmdLine >>= 
       runParseAndEval
