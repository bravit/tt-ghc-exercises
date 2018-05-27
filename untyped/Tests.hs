module Main where

import Control.Monad
import Test.HUnit
import Evaluator
import TaplError
import FullUntyped
import FullUntypedParser
import TestUtils
import UntypedTests
import ArithTests

subTests = [("no sub var",  TmVar 0 1, 1, 
                 TmVar 5 7, TmVar 0 1)
           ,("yes sub var", TmVar 5 7, 1, 
                 TmVar 5 7, TmVar 1 1)
           ,("no sub in abs",  TmAbs "x" (TmVar 0 1), 0, 
                   TmVar 5 7,  TmAbs "x" (TmVar 0 1))
           ,("yes sub in abs", TmAbs "x" (TmVar 6 8), 0, 
                    TmVar 5 7, TmAbs "x" (TmVar 1 1))
           ,("no sub in app",  TmApp (TmVar 0 1) (TmVar 2 3), 1,
                   TmVar 5 7,  TmApp (TmVar 0 1) (TmVar 2 3))
           ,("one sub in app", TmApp (TmVar 5 7) (TmVar 2 3), 0,
                   TmVar 5 7,  TmApp (TmVar 0 1) (TmVar 2 3))
           ,("two sub in app", TmApp (TmVar 5 7) (TmVar 5 7), 2,
                   TmVar 5 7,  TmApp (TmVar 2 1) (TmVar 2 3))
           ]

makeSubTest (label, expected, idx, replacement, term)
    = TestLabel label (TestCase (assertEqual "Incorrect substitution"
                                             expected
                                             (sub idx replacement term)))

shiftTests = [("no shift var",  TmAbs "x" (TmVar 0 3), 
                            2,  TmAbs "x" (TmVar 0 1))
             ,("yes shift var", TmAbs "x" (TmVar 3 6), 
                            2,  TmAbs "x" (TmVar 1 4))
             ,("no shift two abs",   TmAbs "x" (TmAbs "y" (TmVar 1 5)), 
                                4,   TmAbs "x" (TmAbs "y" (TmVar 1 1)))
             ,("yes shift two abs",  TmAbs "x" (TmAbs "y" (TmVar 6 5)), 
                                4,   TmAbs "x" (TmAbs "y" (TmVar 2 1)))
             ,("no shift in app", 
                  TmApp (TmAbs "x" (TmVar 0 4)) (TmAbs "x" (TmVar 0 2)),
               1, TmApp (TmAbs "x" (TmVar 0 3)) (TmAbs "x" (TmVar 0 1)))
             ,("one shift in app", 
                  TmApp (TmAbs "x" (TmVar 3 4)) (TmAbs "x" (TmVar 0 2)),
               1, TmApp (TmAbs "x" (TmVar 2 3)) (TmAbs "x" (TmVar 0 1)))
             ,("two shifts in app", 
                  TmApp (TmAbs "x" (TmVar 3 4)) (TmAbs "x" (TmVar 2 2)),
               1, TmApp (TmAbs "x" (TmVar 2 3)) (TmAbs "x" (TmVar 1 1)))
             ]

makeShiftTest (label, expected, inc, term)
    = TestLabel label (TestCase (assertEqual "Incorrect shift"
                                             expected
                                             (shift inc term)))

parseTests = [("whitespace1", TmBind "x" NameBind, "  \tx/;"),
              ("whitespace2", TmBind "x" NameBind, "x/;   "),
              ("whitespace2", TmBind "x" NameBind, "x / ; "),
              ("comments1", TmBind "x" NameBind, "/* comment */x/;"),
              ("comments2", TmBind "x" NameBind, "/* comment */ x/***** foo */  /; "),
              ("comments3", TmBind "x" NameBind, "/* comment ******/x/;"),
              ("true", TmTrue, "true;"),
              ("true + parens", TmTrue, "(true);"),
              ("false", TmFalse, "false;"),
              ("false + parens", TmFalse, "(false);"),
              ("zero", TmZero, "0;"),
              ("zero + parens", TmZero, "(0);"),
              ("if", TmIf TmTrue TmZero TmFalse, "if true then 0 else false;"),
              ("if + parens", TmIf TmTrue TmZero TmFalse, "(if true then 0 else false);"),
              ("succ", TmSucc TmZero, "succ 0;"),
              ("pred", TmPred TmZero, "pred 0;"),
              ("iszero", TmIsZero TmZero, "iszero 0;"),
              ("combo1", TmSucc (TmPred TmZero), "(succ (pred 0));"),
              ("combo2", TmIsZero (TmSucc (TmPred TmZero)), "iszero (succ (pred 0));"),
              ("abs + app", TmAbs "x" (TmApp (TmVar 0 1) (TmVar 0 1)), 
                              "lambda x. x x;"),
              ("app associativity", 
               TmAbs "x" (TmApp (TmApp (TmVar 0 1) (TmVar 0 1))
                                (TmVar 0 1)),
               "lambda x. x x x;"),
              ("bind", TmBind "x" NameBind, "x/;"),
              ("abs + app", TmAbs "x" (TmApp (TmVar 0 1) (TmVar 0 1)), 
                              "lambda x. x x;"),
              ("app associativity", 
               TmAbs "x" (TmApp (TmApp (TmVar 0 1) (TmVar 0 1))
                                (TmVar 0 1)),
               "lambda x. x x x;")
             ]

fullUntypedParseTests 
    = [("string", TmString "test str", "\"test str\";")
      ,("float", TmFloat 1.2, "1.2;")
      ,("timesfloat", TmTimesfloat (TmFloat 1.1) (TmFloat 1.2), "timesfloat 1.1 1.2;")
      ,("abb bind", TmBind "x" (TmAbbBind (TmFloat 1.2)), "x = 1.2;")
      ,("let", TmLet "x" (TmFloat 1.1) (TmFloat 1.2), "let x = 1.1 in 1.2;")
      ,("record of 0", TmRecord [], "{};")
      ,("record of 1, named", TmRecord [("x", TmFloat 1.1)], "{x=1.1};")
      ,("record of 1, unnamed", TmRecord [("1", TmFloat 1.1)], "{1.1};")
      ,("record of 2, no names", TmRecord [("1", TmFloat 1.1), ("2", TmFloat 1.2)], "{1.1, 1.2};")
      ,("record of 2, named", TmRecord [("x", TmFloat 1.1), ("y", TmFloat 1.2)], "{ x  = 1.1   , y=1.2};")
      ,("proj", TmProj (TmRecord []) "x", "{}.x;")
      ]

fullUntypedEvalTests
    = [("proj record by name", "(lambda x. x)", "{x=lambda x.x, y=lambda z.z}.x;")
      ,("proj record by index 1", "(lambda x. x)", "{lambda x.x, lambda z.z}.1;")
      ,("proj record by index 2", "(lambda z. z)", "{lambda x.x, lambda z.z}.2;")
      ,("proj record + eval", "(lambda z. z z)", "{x=(lambda x.x)(lambda z. z z), y=lambda z.z}.x;")
      ,("proj eval that yields record", "(lambda z. z z)", "((lambda x.x) {lambda z. z z}).1;")
      ,("float", "4.3", "4.3;")
      ,("times float", "1.32", "timesfloat 1.1 1.2;")
      ,("times float after eval", "1.32", "timesfloat 1.1 ((lambda x. 1.2) 3.0);")
      ,("string", "\"str\"", "\"str\";")
      ,("let1", "1.1", "let x = 1.1 in x;")
      ,("let2", "1.2", "let x = 1.1 in 1.2;")
      ,("binder + timesfloat", "x = 1.1\ny = 1.2\n1.32", "x = 1.1; y = 1.2; timesfloat x y;")
      ,("let + timesfloat", "1.32", "let x = 1.1 in (timesfloat x 1.2);")
      ,("two lets", "1.32", "let x = 1.1 in let y = 1.2 in (timesfloat x y);")
      ,("bind assignment", "x = 2.3\n2.3", "x = 2.3; x;")
      ,("bind re-assignment", "x = 2.3\nx = 2.4\n2.4", "x = 2.3; x = 2.4; x;")
      ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat [map makeShiftTest shiftTests
                             , map makeSubTest subTests
                             , map (makeParseTest parseFullUntyped) parseTests
                             , map (makeEvalTest parseAndEval) arithEvalTests
                             , map (makeEvalTest parseAndEval) untypedEvalTests
                             , map (makeParseTest parseFullUntyped) fullUntypedParseTests
                             , map (makeEvalTest parseAndEval) fullUntypedEvalTests
                             , [testDotFTest]
                             ]

main :: IO ()
main = do allTests <- getAllTests
          runTests allTests

