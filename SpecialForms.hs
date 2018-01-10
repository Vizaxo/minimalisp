module SpecialForms where

import Types
import Interpreter
import qualified Data.Map as M

specialForms =
  M.insert "nil"    (Nil)  $
  M.insert "lambda" (SpecialForm lambda) $
  M.insert "eval"   (SpecialForm evalSpecialForm) $
  M.insert "define" (SpecialForm (\env [Symbol s, e] -> (eval' env e, M.insert s (eval' env e) env))) $
  M.insert "quote"  (SpecialForm (\env [a] -> (a, env))) $
  M.insert "if"     (SpecialForm ifSpecialForm) $
  M.empty

evalSpecialForm env [e] = eval env' e'
  where (e', env') = eval env e

lambda env f = (lambda' f, env)
  where lambda' ((Cons args):[]) = Error "Lambda has no body."
        lambda' ((Cons args):body)
          | all isSymbol args    = Lambda (map (\(Symbol s) -> s) args) (Multiple body)
          | otherwise            = Error "Lambda arguments must be symbols."
        lambda' _                = Error "Lambda has no arguments."
        isSymbol (Symbol _) = True
        isSymbol _          = False

ifSpecialForm env (cond:true:false) = eval env chosen
  where chosen = if cond' then true else Multiple false
        cond' = cond'' (eval' env cond)
        cond'' Nil = False
        cond'' _   = True
