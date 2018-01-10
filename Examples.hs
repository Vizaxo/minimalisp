module Examples where

import Types

example1 = Cons [Symbol "quote", Cons [Nil, LispInt 4]]
example2 = Cons [Symbol "eval", example1]
example3 = Cons [Lambda ["a", "b"] (Symbol "b"), LispInt 10, LispInt 5]
example4 = Multiple
           [Cons [Symbol "define", Symbol "first", Lambda ["a", "b"] (Symbol "a")],
            Cons [Symbol "define", Symbol "num", LispInt 3],
            Cons [Symbol "first", Symbol "num", LispInt 2]]
