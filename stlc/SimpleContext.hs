{- A context records the lexically-bound variables.  This SimpleContext
   module is used by the fullsimple implementation, and others based on it.

   de Bruijn indexes are used, as described in Chapter 6.
   Each element of the context record both the name of the variable,
   and the binding.
 -}
module SimpleContext where

import Syntax
import TaplError
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

newtype Context = Ctx [(String, Binding)]
    deriving (Show, Eq)

newContext :: Context
newContext = Ctx []

ctxLength :: Context -> Int
ctxLength (Ctx ps) = length ps

-- to ensure that names are unique, we use the name recorded during parsing
-- only as a suggestion, and keep appending single-quotes to the name
-- until it is unique
pickFreshName :: String -> Context -> String
pickFreshName var (Ctx ps) = iter var ps
    where iter name [] = name
          iter name ((n,_):rest) | n == name = iter (name ++ "'") ps
                                 | otherwise = iter name rest

indexOf :: String -> Context -> ThrowsError Int
indexOf var (Ctx ps) = iter 0 ps
    where iter _ [] = throwError $ UndefinedVariable var
          iter i ((v,b):ps) | v == var  = return i
                            | otherwise = iter (i + 1) ps

nameOf :: Int -> Context -> ThrowsError String
nameOf idx = (liftM fst) . (bindingPairOf idx)

bindingOf :: Int -> Context -> ThrowsError Binding
bindingOf idx = (liftM snd) . (bindingPairOf idx)

bindingPairOf :: Int -> Context -> ThrowsError (String, Binding)
bindingPairOf idx (Ctx ps) 
    = if idx >= length ps
      then throwError $ UndefinedVariable $ "at index " ++ show idx
      else if idx < 0
           then throwError $ Default "Negative index for context"
           else return $ ps !! idx

appendBinding :: String -> Binding -> Context -> Context
appendBinding var binding (Ctx ps) = Ctx $ (var,binding) : ps

-- Monad Transformer to combine ThrowsError with a Context
type ContextThrowsError = ErrorT TaplError (State Context)

liftThrows :: ThrowsError a -> ContextThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runContextThrows :: ContextThrowsError a -> ThrowsError a
runContextThrows action = evalState (runErrorT action) newContext

-- helper function to perform an action with the 
-- (temporary) addition of a binding
withBinding var b action = do ctx <- get
                              put $ appendBinding var b ctx
                              result <- action
                              put ctx
                              return result

-- allows the caller to temporarily use an old context
withContext ctx action = do origCtx <- get
                            put ctx
                            result <- action
                            put origCtx
                            return result

                                        