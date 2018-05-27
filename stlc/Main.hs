{- fullsimple implemenation.  

 Example Usage: "./f -I../ test.f" will try to evaluate the terms in
 the file "test.f", with "../" added to the search path.
 -}
module Main where
    
import Control.Monad
import Util
import Evaluator
import TaplError

parseEvalAndPrint :: String -> IO ()
parseEvalAndPrint = putStrLn . runThrows . parseAndEval

main :: IO ()
main = getContentsFromCmdLine >>= parseEvalAndPrint
