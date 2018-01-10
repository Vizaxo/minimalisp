module Parser where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec hiding (Error)
import Types

parseExpr input = result $ parse expr "" input
  where result (Left err) = Error $ show err
        result (Right e)  = e

expr :: Parser Expr
--expr = (Multiple <$> (list <$> expr <*> ((many space) *> expr))) <|> (sexp <|> integerLit <|> symbol <* (many space))
  --where list a b = [a, b]

--expr = (multiple <$> (singleExpr <*> ((many space) *> expr))) <|> singleExpr
--expr = (multiple <$> (singleExpr <*> singleExpr)) <|> singleExpr
expr = try (multiple <$> singleExpr <* (many space) <*> expr) <|> (many space) *> singleExpr <* (many space)
  where multiple a (Multiple bs) = Multiple (a:bs)
        multiple a b             = Multiple [a, b]

sexp :: Parser Expr
sexp = cons <$> (between (char '(') (char ')')) expr
  where cons (Multiple es) = Cons es
        cons e             = Cons [e]

singleExpr :: Parser Expr
singleExpr = (many space) *> (try nil <|> sexp <|> integerLit  <|> symbol) <* (many space)

nil = Nil <$ (string "(" *> (many space) *> string ")")

integerLit :: Parser Expr
integerLit = LispInt <$> integer

integer :: Parser Int
integer = read <$> ((++) <$> (option "" (string "-")) <*> (many1 digit))

symbol :: Parser Expr
symbol = Symbol <$> (many1 $ noneOf " ()")
