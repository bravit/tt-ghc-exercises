{- Context used to implement de Bruijn indices, which are described in 
   chapter 6.  Unlike the version in untyped (which was a simple list 
   of strings), this one is a list of (binding, string) pairs.
   The list is indexed in reserve order.
 -}
module Context where

import FullUntyped
import TaplError
import Control.Monad.Error

newtype Context = Context [(String, Binding)]

newContext :: Context
newContext = Context []

appendBinding :: Context -> String -> Binding -> Context
appendBinding (Context ns) n binding = Context (ns ++ [(n,binding)])

-- Returns a name not already in use in the context, and binds it to the
-- end of the context, return both the name and the new context as a pair.
-- 
-- Returns the suggested name if it is not already in use in the context.
-- Otherwise, it keeps adding quotes to the end of the name until it
-- finds one that isn't in use.
pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx@(Context bindings) name = 
    let freshName = getName name bindings
    in (appendBinding ctx freshName NameBind, freshName)
    where getName name [] = name
          getName name ((n,_):ns) | n == name = getName (name ++ "'") bindings
                                  | otherwise = getName name ns

getNameByIndex :: Context -> Int -> String
getNameByIndex ctx = fst . (getBindingByIndex ctx)

getValueByIndex :: Context -> Int -> ThrowsError (Maybe Term)
getValueByIndex ctx index = case getBindingByIndex ctx index of
                              (_,NameBind) -> return Nothing
                              (_,TmAbbBind t) -> return $ Just t

getBindingByIndex :: Context -> Int -> (String, Binding)
getBindingByIndex (Context ns) index =
    if length ns > index
    then (reverse ns) !! index
    else error $ "Requested index " ++ show index ++ 
             " of Context of length " ++ show (length ns)

ctxLength :: Context -> Int
ctxLength (Context ns) = length ns

showInCtx :: Term -> Context -> String
showInCtx t@(TmVar _ _) ctx = if contextLength t == ctxLength ctx
                              then getNameByIndex ctx (index t)
                              else error "Context length does match"
showInCtx (TmAbs str t) ctx = let (ctx', name) = pickFreshName ctx str
                              in "(lambda " ++ name ++ ". " ++ 
                                 showInCtx t ctx' ++ ")"
showInCtx (TmApp t1 t2) ctx = showInCtx t1 ctx ++ " " ++ showInCtx t2 ctx
showInCtx TmTrue ctx = "true"
showInCtx TmFalse ctx = "false"
showInCtx (TmIf p c a) ctx = "if " ++ showInCtx p ctx ++ 
                             " then " ++ showInCtx c ctx ++ 
                             " else " ++ showInCtx a ctx
showInCtx TmZero ctx = "0"
showInCtx (TmSucc t) ctx | isnumerical t = showAsNum t 1 
                         | otherwise = "(succ " ++ showInCtx t ctx ++ ")"
                         where showAsNum TmZero num = show num
                               showAsNum (TmSucc t) num = showAsNum t (num + 1)
showInCtx (TmPred t) ctx = "(pred " ++ showInCtx t ctx ++ ")"
showInCtx (TmIsZero t) ctx = "iszero " ++ showInCtx t ctx
showInCtx (TmRecord []) ctx = "{}"
showInCtx (TmRecord recs) ctx = "{" ++ foldl1 (\r1 r2 -> r1 ++ ", " ++ r2) (map showRec recs) ++ "}"
    where showRec (var, term) = var ++ "=" ++ showInCtx term ctx
showInCtx (TmProj term var) ctx = (showInCtx term ctx) ++ "." ++ var
showInCtx (TmFloat num) _ = show num
showInCtx (TmTimesfloat f1 f2) ctx = "timesfloat " ++ showInCtx f1 ctx ++ showInCtx f2 ctx
showInCtx (TmString str) _ = "\"" ++ str ++ "\""
showInCtx (TmLet str term body) ctx
    = let (ctx', name) = pickFreshName ctx str
      in "let " ++ name ++ " = " ++ showInCtx term ctx' ++ 
             " in " ++ showInCtx body ctx'
showInCtx t _ = error $ "Unhandled term: " ++ show t

showTerms :: [Term] -> Context -> String
showTerms [] _ = ""
-- handle TmBind here, because it is the only that affects
-- terms later in the sequence (as opposed to an abstraction,
-- which affect terms within its subtree)
showTerms ((TmBind str binding):ts) ctx
    = (case binding of
         NameBind -> str ++ " "
         TmAbbBind t -> str ++ " = " ++ showInCtx t ctx) ++
      "\n" ++ showTerms ts (appendBinding ctx str binding)
showTerms (t:ts) ctx = showInCtx t ctx ++ "\n" ++ showTerms ts ctx


