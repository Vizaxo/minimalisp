module Examples where

import Types
import Interpreter
import qualified Data.Map as M

defaultEnv = M.insert "eval" (SpecialForm (\env [e] -> eval env e))
  $ M.insert "define" (SpecialForm (\env [Symbol s, e] -> (e, M.insert s e env)))
  $ M.insert "quote" (SpecialForm (\env [a] -> (a, env))) M.empty

example1 = Cons [Symbol "quote", Cons [Nil, LispInt 4]]
example2 = Cons [Symbol "eval", example1]
example3 = Cons [Lambda ["a", "b"] (Symbol "b"), LispInt 10, LispInt 5]
example4 = Multiple
           [Cons [Symbol "define", Symbol "first", Lambda ["a", "b"] (Symbol "a")],
            Cons [Symbol "define", Symbol "num", LispInt 3],
            Cons [Symbol "first", Symbol "num", LispInt 2]]
