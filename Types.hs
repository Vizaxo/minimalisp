module Types where

import qualified Data.Map as M
import Text.Show.Functions

data Expr = Nil
          | Cons [Expr]
          | LispInt Int
          | Symbol SymbolT
          | Lambda [SymbolT] Expr
          | Error String
          | Macro [SymbolT] Expr
          | SpecialForm (Environment -> [Expr] -> (Expr, Environment))
          | Multiple [Expr]
          deriving Show

type SymbolT = String

type Environment = M.Map SymbolT Expr
