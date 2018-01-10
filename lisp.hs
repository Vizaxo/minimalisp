import qualified Data.Map as M

data Expr = Nil
          | Cons [Expr]
          | LispInt Int
          | Symbol SymbolT
          | Lambda [SymbolT] Expr
          | Error String
          deriving Show

type SymbolT = String

type Environment = M.Map SymbolT Expr

eval :: Environment -> Expr -> Expr
eval env Nil                  = Nil
eval env (Cons (s:args)) | isFunction env s = funcall env (eval env s) (map (eval env) args)
eval env (LispInt i)          = LispInt i
eval env (Symbol s)           = Symbol s
eval env (Lambda params body) = undefined
eval env (Error e)            = Error e

funcall :: Environment -> Expr -> [Expr] -> Expr
  --Todo: implement quote as special form
funcall env (Symbol "quote") [x] = x
funcall env (Symbol "quote") []  = Error "Function quote not applied to arguments"
funcall env (Symbol "quote") xs  = Error "Function quote applied to too many arguments"
funcall env (Symbol f) args      = lambdaApply env (symbolLookup env f) args
funcall env e ags                = Error $ "Function not a symbol: " ++ show e

isFunction :: Environment -> Expr -> Bool
isFunction env (Symbol s)   = True
isFunction env (Lambda _ _) = True
isFunction env Nil          = False
isFunction env (LispInt _)  = False
isFunction env (Error _)    = False
isFunction env e            = isFunction env (eval env e)

lambdaApply :: Environment -> Expr -> [Expr] -> Expr
lambdaApply env (Lambda [params] body) args = undefined
lambdaApply _ _ _ = Error "Trying to apply an object which isn't a lambda."

symbolLookup :: Environment -> SymbolT -> Expr
symbolLookup env s = case M.lookup s env of
                       Just e  -> e
                       Nothing -> Error $ "Symbol definition for " ++ show s ++ " not found."
