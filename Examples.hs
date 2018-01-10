module Examples where

import Types
import Interpreter
import qualified Data.Map as M

defaultEnv =
  M.insert "lambda" (SpecialForm lambda) $
  M.insert "eval" (SpecialForm (\env [e] -> eval env e)) $
  M.insert "define" (SpecialForm (\env [Symbol s, e] -> (eval' env e, M.insert s (eval' env e) env))) $
  M.insert "quote" (SpecialForm (\env [a] -> (a, env))) $
  M.empty

lambda env f = (lambda' f, env)
  where lambda' ((Cons args):[]) = Error "Lambda has no body."
        lambda' ((Cons args):body)
          | all isSymbol args    = Lambda (map (\(Symbol s) -> s) args) (Multiple body)
          | otherwise            = Error "Lambda arguments must be symbols."
        lambda' _                = Error "Lambda has no arguments."
        isSymbol (Symbol _) = True
        isSymbol _          = False

example1 = Cons [Symbol "quote", Cons [Nil, LispInt 4]]
example2 = Cons [Symbol "eval", example1]
example3 = Cons [Lambda ["a", "b"] (Symbol "b"), LispInt 10, LispInt 5]
example4 = Multiple
           [Cons [Symbol "define", Symbol "first", Lambda ["a", "b"] (Symbol "a")],
            Cons [Symbol "define", Symbol "num", LispInt 3],
            Cons [Symbol "first", Symbol "num", LispInt 2]]
