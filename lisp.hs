{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import qualified Data.Map as M
import Text.Show.Functions

data Expr = Nil
          | Cons [Expr]
          | LispInt Int
          | Symbol SymbolT
          | Lambda [SymbolT] Expr
          | Error String
          | Macro [SymbolT] Expr
          | SpecialForm (Environment -> [Expr] -> Expr)
          deriving Show

type SymbolT = String

type Environment = M.Map SymbolT Expr

defaultEnv = M.insert "eval" (SpecialForm (\env [a] -> eval env a)) --Placeholder for eval
  $ M.insert "quote" (SpecialForm (\_ [a] -> a)) M.empty

example = Cons [Symbol "quote", Cons [Nil, LispInt 4]]
example2 = Cons [Symbol "eval", example]

eval :: Environment -> Expr -> Expr
eval env (Cons (e:args))
  | isFunction env e'      = funcall env e' (map (eval env) args)
  | isMacro env e'         = eval env $ macroExpand env e' args
  | isSpecialForm env e'   = apply e'
  | otherwise              = Error $ "Could not apply expression " ++ show e
  where apply (SpecialForm f) = f env args
        apply _               = Error "Not special form"
        e' = eval env e
eval env (Symbol s)        = symbolLookup env s
eval _   e@Nil             = e
eval _   e@(LispInt _)     = e
eval _   e@(Lambda _ _)    = e
eval _   e@(Error _)       = e
eval _   e@(SpecialForm _) = e

funcall :: Environment -> Expr -> [Expr] -> Expr
funcall env e args = lambdaApply env (eval env e) args

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
lambdaApply env (Lambda [params] body) args = undefined
lambdaApply _   _                      _    = Error "Trying to apply an object which isn't a lambda."

symbolLookup :: Environment -> SymbolT -> Expr
symbolLookup env s = case M.lookup s env of
                       Just e  -> e
                       Nothing -> Error $ "Symbol definition for " ++ show s ++ " not found."

macroExpand :: Environment -> Expr -> [Expr] -> Expr
macroExpand = undefined
