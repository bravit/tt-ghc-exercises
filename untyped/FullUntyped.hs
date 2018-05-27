{- Contains data structures and printing functions for untyped
   lambda calculus.
-}
module FullUntyped where

data Binding = NameBind
             | TmAbbBind Term
               deriving (Show, Eq)

-- TmVar records both its de Bruijn index and the length of the context
-- it was defined in (as described in Chapter 7, this helps expose
-- implementation errors).  
--
-- TmAbs stores the variable name (e.g., "x" in "lambda x. ...")
-- only so that we can try to use that name when printing the 
-- function (it does not affect how the abstraction is evaluated)
data Term = TmVar { index :: Int, contextLength :: Int }
          | TmAbs String Term
          | TmApp Term Term
          | TmBind String Binding
          -- from arith
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term  
          -- new
          | TmRecord [(String, Term)]
          | TmProj Term String
          | TmFloat Double
          | TmTimesfloat Term Term
          | TmString String
          | TmLet String Term Term
            deriving (Show, Eq)

isval :: Term -> Bool
isval TmTrue = True
isval TmFalse = True
isval TmZero = True
isval (TmSucc _) = True
isval (TmFloat _) = True
isval (TmString _) = True
isval (TmAbs _ _) = True
isval (TmRecord fs) = and $ map (\(_,v) -> isval v) fs
isval _           = False
     
isnumerical :: Term -> Bool
isnumerical TmZero = True
isnumerical (TmSucc t) = isnumerical t
isnumerical _ = False
