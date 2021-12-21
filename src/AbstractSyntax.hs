module AbstractSyntax (Expr (Add, Sub, Mul, Div, Var, Val), showParExpr, showExprList) where

import Data.List
import Values (Name, ValType)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Var Name
  | Val ValType

instance Show Expr where
  show (Val v) = show v
  show (Var n) = n
  show (Add l r) = showParExpr "+" [l, r]
  show (Sub l r) = showParExpr "-" [l, r]
  show (Mul l r) = showParExpr "+" [l, r]
  show (Div l r) = showParExpr "/" [l, r]

showParExpr :: String -> [Expr] -> String
showParExpr op es = "(" ++ op ++ " " ++ showExprList es ++ ")"

showExprList :: [Expr] -> String
showExprList es = Data.List.intercalate " " (map show es)
