{- Provides the methods for determining the type of a term or a binding
 -}
module Typing where

import Syntax
import SimpleContext
import TaplError
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

checkSubtype :: Term -> Ty -> Ty -> ContextThrowsError Ty
checkSubtype t expected output
    = do tyT <- typeof t
         if subtype tyT expected
           then return output
           else throwError $ TypeMismatch $ "Expected " ++ show expected ++
                ", but saw " ++ show tyT

{- -------------------------------------
   typeof
 ------------------------------------- -}

typeof :: Term -> ContextThrowsError Ty
typeof TmTrue  = return TyBool
typeof TmFalse = return TyBool
typeof TmZero  = return TyNat
typeof TmUnit  = return TyUnit
typeof (TmFloat _)  = return TyFloat
typeof (TmTimesFloat t1 t2) = checkSubtype t1 TyFloat TyFloat >>
                              checkSubtype t2 TyFloat TyFloat
typeof (TmString _) = return TyString
typeof (TmSucc t)   = checkSubtype t TyNat TyNat
typeof (TmPred t)   = checkSubtype t TyNat TyNat
typeof (TmIsZero t) = checkSubtype t TyNat TyBool
typeof (TmIf p c a) = do tyP <- typeof p
                         if tyP /= TyBool
                           then throwError expectedBool
                           else do tyC <- typeof c
                                   tyA <- typeof a
                                   joinOrThrow tyC tyA
typeof (TmBind v TyVarBind) = return $ TyId v
typeof (TmBind v b) = do modify $ appendBinding v b
                         liftThrows $ typeOfBinding b
typeof (TmAscribe t ty) = do tyT <- typeof t
                             if subtype tyT ty
                               then return ty
                               else throwError ascribeError
typeof (TmVar idx _) = do ctx <- get
                          b <- liftThrows $ bindingOf idx ctx
                          liftThrows $ typeOfBinding b
typeof (TmAbs var ty body) = withBinding var (VarBind ty) $ 
                             liftM (TyArr ty) $ typeof body 
typeof (TmLet var t body)  = do ty <- typeof t
                                withBinding var (VarBind ty) $ typeof body
typeof (TmApp t1 t2) 
    = do tyT1 <- typeof t1
         tyT2 <- typeof t2
         case tyT1 of
           (TyArr _ _) -> checkTyArr tyT1 tyT2
           (TyVar _)   -> return tyT2
           TyBot     -> return TyBot
           otherwise -> throwError notAbstraction
    where checkTyArr (TyArr tyArr1 tyArr2) tyT2
              | subtype tyT2 tyArr1 = return tyArr2
              | otherwise           = throwError badApplication 
typeof (TmRecord fs) = liftM TyRecord $ mapM typeofField fs
    where typeofField (n,t) = do ty <- typeof t
                                 return (n, ty)
typeof (TmProj r name) = do recordTy <- typeof r
                            case recordTy of
                              TyRecord fs -> accessField name fs
                              otherwise -> throwError projError
typeof (TmTag _ _ ty) = return ty
typeof (TmCase t ((label,_):cs)) = do (TyVariant fs) <- typeof t
                                      accessField label fs
typeof (TmInert ty) = return ty
typeof (TmFix t) = do ty <- typeof t
                      case ty of
                        TyArr t1 t2 | subtype t2 t1 -> return t2
                        otherwise -> throwError fixError
typeof _ = throwError $ Default "Unknown type"

accessField name [] = throwError $ TypeMismatch $ "No field " ++ name
accessField name ((n,t):fs) | n == name = return t
                            | otherwise = accessField name fs

typeofTerms :: [Term] -> ThrowsError [Ty]
typeofTerms = runContextThrows . mapM typeof

{- -------------------------------------
   typeofBinding
 ------------------------------------- -}

typeOfBinding :: Binding -> ThrowsError Ty
typeOfBinding (VarBind ty) = return ty
typeOfBinding (TmAbbBind _ (Just ty)) = return ty
typeOfBinding (TyAbbBind ty) = return ty
typeOfBinding _ = throwError $ Default "No type information exists"

{- -------------------------------------
   subtype -- this is a placeholder that
   will be replaced when we have subtypes
 ------------------------------------- -}

subtype :: Ty -> Ty -> Bool
subtype = (==)
 

{- -------------------------------------
   join -- this is a placeholder that
   will be replaced when we have subtypes 
 ------------------------------------- -}

joinOrThrow :: Ty -> Ty -> ContextThrowsError Ty
joinOrThrow ty1 ty2 = if ty1 == ty2
                 then return ty1
                 else throwError ifMismatch
 
