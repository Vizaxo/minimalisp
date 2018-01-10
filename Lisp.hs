module Lisp where

import Types
import Parser
import Interpreter
import Examples
import SpecialForms

interpret = eval' specialForms . parseExpr
