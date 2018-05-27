module TaplError where
{-
 - Adapted from the excellent tutorial:
 - http://halogen.note.amherst.edu/~jdtang/scheme_in_48/tutorial/overview.html
-}

import Control.Monad.Error

data TaplError = ParserError String
               | EvalError String
               | TypeMismatch String
               | UndefinedVariable String
               | Default String
               
instance Show TaplError where
    show (ParserError msg) = "Parse Error: " ++ msg
    show (EvalError msg) = "Evaluation Error: " ++ msg
    show (TypeMismatch msg) = "Type Mismatch: " ++ msg
    show (UndefinedVariable msg) = "Undefined variable: " ++ msg
    show (Default msg) = "Error: " ++ msg

instance Error TaplError where
    noMsg  = Default "An unknown error has occurred"
    strMsg = Default

type ThrowsError = Either TaplError

trapError = (flip catchError) (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

runThrows :: ThrowsError String -> String
runThrows action = extractValue $ trapError action

-- some common errors
badApplication = TypeMismatch "Invalid argument passed to an abstraction"
notAbstraction = TypeMismatch "First term of application must be an abstraction"
projError = TypeMismatch "A projection can only be applied to a record"
expectedBool = TypeMismatch "The conditional of an if-statement must be a Bool"
ifMismatch = TypeMismatch "Predicate and alternative of an if-statement must be of the same type"
ascribeError = TypeMismatch "body of as-term is not a subtype"
fixError = TypeMismatch "result of body not compatible with domain"

