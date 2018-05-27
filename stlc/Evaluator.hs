    
{- Small-step evaluator for fullsimple 
 -}
module Evaluator ( parseAndEval ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Syntax
import SimpleContext
import Typing
import Parser
import TaplError
import Printing

{- ---------------------------------
 sub/shift
 --------------------------------- -}

sub i val t = walk 0 t subVar
    where subVar c v@(TmVar idx _) | c + i == idx = shift c val
                                   | otherwise    = v

shift i t = walk 0 t shiftVar
    where shiftVar c (TmVar idx ctxLen) 
              | idx >= c  = TmVar (idx + i) (ctxLen + i)
              | otherwise = TmVar idx (ctxLen + i)

-- helper function abstracting the common functionality of sub/shift
walk c t f = case t of
               TmVar _ _ -> f c t
               TmAbs var ty body -> TmAbs var (walkType c ty f) 
                                    (walk (c + 1) body f)
               TmLet var t body -> TmLet var (walk c t f) 
                                   (walk (c + 1) body f)
               TmApp t1 t2 -> TmApp (walk c t1 f) (walk c t2 f)
               TmSucc t -> TmSucc $ walk c t f
               TmPred t -> TmPred $ walk c t f
               TmIsZero t -> TmIsZero $ walk c t f
               TmIf t1 t2 t3 -> TmIf (walk c t1 f) (walk c t2 f) (walk c t3 f)
               TmTimesFloat t1 t2 -> TmTimesFloat (walk c t1 f) (walk c t2 f)
               TmAscribe t ty -> TmAscribe (walk c t f) (walkType c ty f)
               TmProj t i -> TmProj (walk c t f) i
               TmRecord fs -> TmRecord $ map (\(n,t) -> (n, (walk c t f))) fs
               TmCase t branches -> TmCase (walk c t f) $
                                    map (\(n,(v,t)) -> 
                                         (n,(v, (walk (c+1) t f)))) branches
               TmTag v t ty -> TmTag v (walk c t f) (walkType c ty f)
               TmInert ty -> TmInert $ walkType c ty f
               TmFix t -> TmFix $ walk c t f
               otherwise -> t

walkType c ty f = case ty of
                    TyVar v -> TyVar $ f c v
                    TyArr ty1 ty2 -> TyArr (walkType c ty1 f)
                                     (walkType c ty2 f)
                    TyRecord  fields -> TyRecord  $ walkFields fields
                    TyVariant fields -> TyVariant $ walkFields fields
                    otherwise -> ty
    where walkFields = map (\(n,ty) -> (n, walkType c ty f))

{- ---------------------------------
 eval1 helper functions
 --------------------------------- -}

eval1Cons :: (Term -> Term) -> Term -> ContextThrowsError (Maybe Term)
eval1Cons constructor = (liftM (liftM constructor)) . eval1

apply term body = shift (-1) $ sub 0 (shift 1 term) body

{- ---------------------------------
 eval1, which executes a single "small step".  Use the Monad Transformer
 ContextThrowsError to implicitly pass around the context during the 
 evaluation.  Return a "Maybe Term", with "Nothing" indicating that
 the input term could not be further reduced.
 --------------------------------- -}

eval1 :: Term -> ContextThrowsError (Maybe Term)
eval1 (TmSucc t)   | isval t   = return Nothing
                   | otherwise = eval1Cons TmSucc t
eval1 (TmPred TmZero)          = return $ Just TmZero
eval1 (TmPred (TmSucc t)) 
    | isnumericval t           = return $ Just t
eval1 (TmPred t)   | isval t   = return Nothing
                   | otherwise = eval1Cons TmPred t
eval1 (TmIsZero TmZero)        = return $ Just TmTrue
eval1 (TmIsZero t) | isval t   = return $ Just TmFalse
                   | otherwise = eval1Cons TmIsZero t
eval1 (TmIf TmTrue  c a)       = return $ Just c
eval1 (TmIf TmFalse c a)       = return $ Just a
eval1 (TmIf p c a) | isval p   = return Nothing 
                   | otherwise = eval1Cons (\p' -> TmIf p' c a) p
eval1 (TmTimesFloat (TmFloat f1) (TmFloat f2))
                     = return $ Just $ TmFloat $ f1 * f2
eval1 (TmTimesFloat t1@(TmFloat _) t2) 
    | not $ isval t2 = eval1Cons (TmTimesFloat t1) t2
eval1 (TmTimesFloat t1 t2) 
    | not $ isval t1 = eval1Cons ((flip TmTimesFloat) t2) t1
eval1 (TmAscribe t _) = return $ Just t
eval1 t@(TmBind var binding) = modify (appendBinding var binding) >>
                               return Nothing
eval1 (TmVar idx ctxLen) = do ctx <- get
                              binding <- liftThrows $ bindingOf idx ctx 
                              case binding of
                                TmAbbBind val _ -> return $ Just val
                                otherwise       -> return Nothing
eval1 (TmApp t1@(TmAbs _ _ body) t2) 
    | isval t2  = return $ Just $ apply t2 body
    | otherwise = eval1Cons (TmApp t1) t2
eval1 (TmApp t1 t2) | not $ isval t1 = eval1Cons ((flip TmApp) t2) t1
eval1 (TmLet var t body) | isval t   = return $ Just $ apply t body
                         | otherwise = eval1Cons (\t' -> TmLet var t' body) t
eval1 (TmRecord fs) = liftM (liftM TmRecord) $ iter fs
    where iter :: [(String,Term)] -> ContextThrowsError (Maybe [(String,Term)])
          iter [] = return Nothing
          iter ((n,t):fs) | isval t   = liftM (liftM ((n,t): )) $ iter fs
                          | otherwise = liftM (liftM (\t' -> ((n,t'):fs))) $ eval1 t
eval1 (TmProj r name) 
    | not $ isval r = eval1Cons (\r' -> TmProj r' name) r
eval1 (TmProj r@(TmRecord fs) name) 
    | isval r = access name fs
    where access name [] = throwError $ EvalError $ 
                           "Field " ++ name ++ " does not exist"
          access name ((n,t):fs) | n == name = return $ Just t
                                 | otherwise = access name fs
eval1 (TmCase t fs) | not $ isval t = eval1Cons ((flip TmCase) fs) t
eval1 (TmCase (TmTag var t _) fs) = branch fs
    where branch [] = throwError $ EvalError "No applicable branch"
          branch ((label, (name, body)):fs)
              | label == var = return $ Just $ apply t body
              | otherwise    = return Nothing
eval1 (TmTag var t ty) | isval t   = return Nothing
                       | otherwise = eval1Cons (\t' -> TmTag var t' ty) t
eval1 (TmFix t) | not $ isval t     = eval1Cons TmFix t
eval1 t@(TmFix (TmAbs var ty body)) = return $ Just $ apply t body
eval1 _ = return Nothing

{- ---------------------------------
 Full evaluation
 --------------------------------- -}

eval :: Term -> ContextThrowsError Term
eval t = do mt' <- eval1 t
            case mt' of
              Nothing -> return t
              Just t' -> eval t'           

evalTerms :: [Term] -> ThrowsError [Term]
evalTerms ts = runContextThrows $ mapM eval ts

parseAndEval :: String -> ThrowsError String
parseAndEval str = do parsed <- parseFullSimple str
                      evaled <- evalTerms parsed
                      typed  <- typeofTerms parsed
                      showTerms evaled typed
