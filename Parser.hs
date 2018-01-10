module Parser where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec hiding (Error)
import Types

parseExpr input = result $ parse multipleExpr "" input
  where result (Left err) = Error $ show err
        result (Right e)  = e

multipleExpr = Multiple <$> many1 expr

expr :: Parser Expr
expr = (many space) *> singleExpr <* (many space)

sexp :: Parser Expr
sexp = between (char '(') (char ')') $ Cons <$> (many1 expr) <|> Nil <$ (many space)

singleExpr :: Parser Expr
singleExpr = (many space) *> (try nil <|> sexp <|> integerLit  <|> symbol) <* (many space)

nil = Nil <$ (string "(" *> (many space) *> string ")")

integerLit :: Parser Expr
integerLit = LispInt <$> integer

integer :: Parser Int
integer = read <$> ((++) <$> (option "" (string "-")) <*> (many1 digit))

symbol :: Parser Expr
symbol = Symbol <$> (many1 $ noneOf " ()")
