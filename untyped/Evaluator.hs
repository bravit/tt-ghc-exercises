module Evaluator (evalTerms, parseAndEval, sub, shift) where

import Control.Monad

import TaplError
import FullUntypedParser
import FullUntyped
import Context

sub :: Int -> Term -> Term -> Term
sub subIdx replacement term = helper term varSub
    where varSub c v@(TmVar _ _) 
              | index v == c + subIdx = shift c replacement
              | otherwise             = v

shift :: Int -> Term -> Term
shift inc term = helper term varShift
    where varShift c (TmVar idx len)
              | idx >= c  = TmVar (idx + inc) (len + inc)
              | otherwise = TmVar idx         (len + inc)

-- contains common functionality of "sub" and "shift"
helper :: Term -> (Int -> Term -> Term) -> Term
helper term varHandler = walk 0 term
    where walk c v@(TmVar _ _)    = varHandler c v
          walk c (TmApp t1 t2)    = TmApp (walk c t1) (walk c t2)
          walk c (TmAbs name t12) = TmAbs name $ walk (c + 1) t12
          walk c TmTrue = TmTrue
          walk c TmFalse = TmFalse
          walk c (TmIf t1 t2 t3) = TmIf (walk c t1) (walk c t2) (walk c t3)
          walk c (TmProj t1 str) = TmProj (walk c t1) str
          walk c (TmRecord fields) = TmRecord $ map (\(v,t) -> (v, walk c t)) fields
          walk c (TmLet str t1 t2) = TmLet str (walk c t1) (walk c t2)
          walk c t@(TmFloat _) = t
          walk c (TmTimesfloat t1 t2) = TmTimesfloat (walk c t1) (walk c t2)
          walk c t@(TmString _) = t
          walk c TmZero = TmZero
          walk c (TmSucc t) = TmSucc $ walk c t
          walk c (TmPred t) = TmPred $ walk c t
          walk c (TmIsZero t) = TmIsZero $ walk c t

-- Recursively call eval1, and construct a Term fro the output.    
-- Uses two liftMs to get inside both the ThrowsError and Maybe monads
eval1Cons :: (Term -> Term) -> Term -> Context -> ThrowsError (Maybe Term)
eval1Cons f t ctx = liftM (liftM f) (eval1 t ctx)

apply t1 t2 = shift (-1) (sub 0 (shift 1 t2) t1)

eval1 :: Term -> Context -> ThrowsError (Maybe Term)
eval1 (TmApp (TmAbs _ t12) t2) ctx | isval t2 = return $ Just $ apply t12 t2
eval1 (TmApp t1 t2) ctx | isval t1  = eval1Cons (TmApp t1) t2 ctx
                        | otherwise = eval1Cons ((flip TmApp) t2) t1 ctx
eval1 (TmIf TmTrue conseq _) ctx = return $ Just conseq
eval1 (TmIf TmFalse _ alt) ctx   = return $ Just alt
eval1 (TmIf pred conseq alt) ctx = eval1Cons (\x -> TmIf x conseq alt) pred ctx
eval1 (TmSucc t) ctx             = eval1Cons TmSucc t ctx
eval1 (TmPred TmZero) ctx        = return $ Just TmZero
eval1 (TmPred (TmSucc nv)) ctx 
    | isnumerical nv             = return $ Just nv
eval1 (TmPred t) ctx             = eval1Cons TmPred t ctx
eval1 (TmIsZero TmZero) ctx      = return $ Just TmTrue
eval1 (TmIsZero (TmSucc nv)) ctx
    | isnumerical nv             = return $ Just TmFalse
eval1 (TmIsZero t) ctx           = eval1Cons TmIsZero t ctx
eval1 t@(TmVar _ _) ctx = if contextLength t == ctxLength ctx
                          then getValueByIndex ctx (index t)
                          else error "Context length is wrong for TmVar"
eval1 (TmTimesfloat (TmFloat f1) (TmFloat f2)) _ = return $ Just $ TmFloat (f1 * f2)
eval1 (TmTimesfloat t1@(TmFloat _) t2) ctx = eval1Cons (TmTimesfloat t1) t2 ctx
eval1 (TmTimesfloat t1 t2) ctx = eval1Cons ((flip TmTimesfloat) t2) t1 ctx
eval1 (TmLet str term body) ctx | isval term = return $ Just $ apply body term
                                | otherwise  = eval1Cons (\t' -> TmLet str t' body) term ctx
eval1 (TmProj (TmRecord fields) str) ctx 
      = case lookup str fields of
          Just term -> return $ Just term
          Nothing -> return Nothing
eval1 (TmProj term str) ctx = eval1Cons ((flip TmProj) str) term ctx
eval1 (TmRecord fields) ctx = liftM (liftM TmRecord) (evalOneField fields)
    where evalOneField :: [(String, Term)] -> ThrowsError (Maybe [(String, Term)])
          evalOneField [] = return Nothing
          evalOneField ((v,f):fs) | isval f   = liftM (liftM ((v,f):)) (evalOneField fs)
                                  | otherwise = liftM (liftM (\f' -> ((v,f'):fs))) (eval1 f ctx)
eval1 _ _ = return Nothing

-- Small-step evaluation of a term
eval :: Term -> Context -> ThrowsError (Term,Context)
-- handle TmBind here, because it is the only that affects
-- terms later in the sequence (as opposed to an abstraction,
-- which affect terms within its subtree)
eval t@(TmBind str binding) ctx = return (t, appendBinding ctx str binding)
eval t ctx = do mt' <- eval1 t ctx
                case mt' of
                  Just t' -> eval t' ctx
                  Nothing -> return (t,ctx)

-- Evaluates a sequence of terms, where earlier terms in
-- the sequence (such as binders) affect the context of later terms.
evalTerms :: [Term] -> Context -> ThrowsError [Term]
evalTerms [] _ = return []
evalTerms (t:ts) ctx = do (t', ctx') <- eval t ctx
                          ts' <- evalTerms ts ctx'
                          return (t':ts')

parseAndEval :: String -> ThrowsError String
parseAndEval str = do parsed <- parseFullUntyped str
                      evaled <- evalTerms parsed newContext
                      return $ showTerms evaled newContext
