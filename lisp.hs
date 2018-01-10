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
eval env Nil                  = Nil
eval env (Cons ((SpecialForm f):args)) = f env args
eval env (Cons (s@(Symbol sText):args)) | isFunction env s = funcall env (eval env s) (map (eval env) args)
                         | isMacro env s    = eval env $ macroExpand env (eval env s) args
                         | isSpecialForm env s = apply (symbolLookup env sText)
                         where apply (SpecialForm f) = f env args
                               apply expr = Error "Not special form"
eval env (LispInt i)          = LispInt i
eval env (Lambda params body) = undefined
eval env (Error e)            = Error e
eval env (SpecialForm f)      = SpecialForm f
eval env (Symbol s)           = symbolLookup env s

funcall :: Environment -> Expr -> [Expr] -> Expr
funcall env (Symbol f) args      = lambdaApply env (symbolLookup env f) args
funcall env e ags                = Error $ "Function not a symbol: " ++ show e

isFunction :: Environment -> Expr -> Bool
isFunction env e@(Cons _)   = isFunction env (eval env e)
isFunction env (Symbol s)   = case symbolLookup env s of
                                Symbol _ -> True
                                _        -> False
isFunction _   (Lambda _ _) = True
isFunction _   _            = False

isMacro :: Environment -> Expr -> Bool
isMacro env e@(Cons _)   = isMacro env (eval env e)
isMacro env (Symbol s)   = case symbolLookup env s of
                             Macro _ _ -> True
                             _         -> False
isMacro _   (Macro _ _)  = True
isMacro _   _            = False

isSpecialForm :: Environment -> Expr -> Bool
isSpecialForm env e@(Cons _)      = isSpecialForm env (eval env e)
isSpecialForm env (Symbol s)      = case symbolLookup env s of
                                      SpecialForm _ -> True
                                      _             -> False
isSpecialForm _   (SpecialForm _) = True
isSpecialForm _   _               = False

lambdaApply :: Environment -> Expr -> [Expr] -> Expr
lambdaApply env (Lambda [params] body) args = undefined
lambdaApply _ _ _ = Error "Trying to apply an object which isn't a lambda."

symbolLookup :: Environment -> SymbolT -> Expr
symbolLookup env s = case M.lookup s env of
                       Just e  -> e
                       Nothing -> Error $ "Symbol definition for " ++ show s ++ " not found."

macroExpand :: Environment -> Expr -> [Expr] -> Expr
macroExpand = undefined
