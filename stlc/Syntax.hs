{- Defines the terms, types, and bindings used in the fullsimple
   implemenation, and provides a couple simple helper functions.
 -}

module Syntax where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import TaplError    

{- --------------------------------
   TERMS
   -------------------------------- -}

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmCase Term [(String, (String, Term))]
          | TmTag String Term Ty
          | TmVar Int Int
          | TmAbs String Ty Term
          | TmApp Term Term
          | TmLet String Term Term
          | TmFix Term
          | TmString String
          | TmUnit
          | TmAscribe Term Ty
          | TmRecord [(String, Term)]
          | TmProj Term String
          | TmFloat Double
          | TmTimesFloat Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | TmInert Ty
          | TmBind String Binding
          deriving (Show, Eq)

isnumericval :: Term -> Bool
isnumericval TmZero          = True
isnumericval (TmSucc t)      = isnumericval t
isnumericval (TmAscribe t _) = isnumericval t
isnumericval _               = False

isval :: Term -> Bool
isval TmTrue             = True
isval TmFalse            = True
isval TmUnit             = True
isval (TmFloat _)        = True
isval (TmString _)       = True
isval (TmAbs _ _ _)      = True
isval (TmAscribe t _)    = isval t
isval (TmRecord fs)      = and $ map (\(_,t) -> isval t) fs
isval (TmTag _ t _)      = isval t
isval t | isnumericval t = True
        | otherwise      = False

{- --------------------------------
   TYPES
   -------------------------------- -}

data Ty = TyVar Term -- the Term will always be a TmVar (a hack to reuse TmVar code)
        | TyId String
        | TyUnit
        | TyArr Ty Ty -- "t1 -> t2"
        | TyRecord [(String, Ty)]
        | TyVariant [(String, Ty)]
        | TyBool
        | TyString
        | TyFloat
        | TyNat
        | TyTop
        | TyBot
          deriving (Show, Eq)

{- --------------------------------
   BINDING
   -------------------------------- -}

data Binding = NameBind
             | TyVarBind
             | VarBind Ty
             | TmAbbBind Term (Maybe Ty)
             | TyAbbBind Ty
               deriving (Show, Eq)
