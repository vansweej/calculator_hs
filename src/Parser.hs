module Parser (pParser) where

import AbstractSyntax (Expr (..))
import Control.Applicative hiding (many)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import GHC.Base (RuntimeRep (AddrRep))
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- expression ::= term { addop term } ;
-- term ::= factor { mulop factor } ;
-- factor ::= "(" expression ")" | NUMBER ;
-- addop ::= "+" | "-" ;
-- mulop ::= "*" ;

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    empty -- (3)
    empty -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: [Char] -> Parser [Char]
symbol = L.symbol sc

pNumber :: Parser Int
pNumber = lexeme L.decimal

pParens :: Parser a -> Parser a
pParens = between (symbol "(") (symbol ")")

pFactor :: Parser Expr
pFactor = pParens pExpression <|> Val <$> pNumber

pTerm :: Parser Expr
pTerm = do
  m1 <- pFactor
  m2 <- many pTermexpr
  return (foldl (Mul) m1 m2)

pTermexpr :: Parser Expr
pTermexpr = do
  lexeme (void (char '*'))
  pFactor

pExpression :: Parser Expr
pExpression = do
  e1 <- pTerm
  e2 <- many pExpressionexpr
  return (foldl (\x y -> (snd y) x (fst y)) e1 e2)

pExpressionexpr :: Parser (Expr, (Expr -> Expr -> Expr))
pExpressionexpr = do
  op <- pPlusOrSubtractExpr
  term <- pTerm
  return (term, op)

pPlusOrSubtractExpr :: Parser (Expr -> Expr -> Expr)
pPlusOrSubtractExpr = pPlusExpr <|> pSubtractExpr

pPlusExpr :: Parser (Expr -> Expr -> Expr)
pPlusExpr = do
  lexeme (void (char '+'))
  return Add

pSubtractExpr :: Parser (Expr -> Expr -> Expr)
pSubtractExpr = do
  lexeme (void (char '-'))
  return Sub

pParser :: Parser Expr
pParser = pExpression <* eof
