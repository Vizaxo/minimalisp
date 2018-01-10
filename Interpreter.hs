module Interpreter where

import Types
import qualified Data.Map as M

eval :: Environment -> Expr -> (Expr, Environment)
eval env (Multiple [])     = (Error "Trying to evaluate 0 expressions.", env)
eval env (Multiple [e])    = eval env' e
  where (_, env') = eval env e
eval env (Multiple (e:es)) = eval env' (Multiple es)
  where (_, env') = eval env e
eval env (Cons (e:args))
  | isFunction    env e'   = (lambdaApply env e' (map (eval' env) args), env)
  | isMacro       env e'   = eval env $ macroExpand env e' (Multiple args)
  | isSpecialForm env e'   = apply e'
  | otherwise              = (Error $ "Could not apply expression " ++ show e, env)
  where apply (SpecialForm f) = f env args
        e' = eval' env e
eval env (Symbol s)        = (symbolLookup env s, env)
eval env e@Nil             = (e, env)
eval env e@(LispInt _)     = (e, env)
eval env e@(Lambda _ _)    = (e, env)
eval env e@(Error _)       = (e, env)
eval env e@(SpecialForm _) = (e, env)

eval' :: Environment -> Expr -> Expr
eval' env = fst . (eval env)

isFunction :: Environment -> Expr -> Bool
isFunction _   (Lambda _ _) = True
isFunction _   _            = False

isMacro :: Environment -> Expr -> Bool
isMacro _   (Macro _ _) = True
isMacro _   _           = False

isSpecialForm :: Environment -> Expr -> Bool
isSpecialForm _   (SpecialForm _) = True
isSpecialForm _   _               = False

lambdaApply :: Environment -> Expr -> [Expr] -> Expr
lambdaApply env (Lambda (p:ps) body) (a:as) = lambdaApply (M.insert p a env) (Lambda ps body) as
lambdaApply env (Lambda []     body) []     = eval' env body
lambdaApply env (Lambda []     body) _      = Error "Trying to apply a lambda to too many arguments."
lambdaApply env (Lambda _      body) []     = Error "Trying to apply a lambda to too few arguments."
lambdaApply _   _                    _      = Error "Trying to apply an object which isn't a lambda."

symbolLookup :: Environment -> SymbolT -> Expr
symbolLookup env s = case M.lookup s env of
                       Just e  -> e
                       Nothing -> Error $ "Symbol definition for " ++ s ++ " not found."

macroExpand :: Environment -> Expr -> Expr -> Expr
macroExpand = undefined
