module EvalCalc
  ( ValType,
    Name,
    Expr (..),
    Env,
    EvalErr,
    eval,
    eval2,
    eval3,
    lastVal,
    newEnviron,
    showEnviron,
    getNameBinding,
    hasNameBinding,
    newNameBinding,
    setNameBinding,
  )
where

import AbstractSyntax (Expr (..))
import Control.Monad.State
import Data.Maybe
import Data.Stack
import Environments
  ( AnEnv,
    bindList,
    getBinding,
    hasBinding,
    newBinding,
    newEnv,
    setBinding,
    toList,
  )
import GHC.Fingerprint (fingerprint0)
import Values (Name, ValType, defaultVal)

type EvalErr = String

type Env = AnEnv ValType

lastVal :: Name
lastVal = "it"

newEnviron :: Env
newEnviron = newBinding lastVal defaultVal newEnv

getNameBinding :: Name -> Env -> Maybe ValType
getNameBinding n env = getBinding n env

hasNameBinding :: Name -> Env -> Bool
hasNameBinding n env = hasBinding n env

newNameBinding :: Name -> ValType -> Env -> Env
newNameBinding n v env = newBinding n v env

setNameBinding :: Name -> ValType -> Env -> Env
setNameBinding n v env = setBinding n v env

showEnviron :: Env -> String
showEnviron env =
  concatMap
    (\(n, v) -> n ++ "\t" ++ show v ++ "\n")
    (toList env)

eval :: Expr -> Env -> Either EvalErr ValType
eval (Val v) _ = Right v
eval (Var n) env =
  case getBinding n env of
    Nothing -> Left ("Undefined variable " ++ n)
    Just i -> Right i
eval (Add l r) env =
  case (eval l env, eval r env) of
    (Right lv, Right rv) -> Right (lv + rv)
    (Left le, Left re) -> Left (le ++ "\n" ++ re)
    (x@(Left le), _) -> x
    (_, y@(Left le)) -> y
eval (Sub l r) env =
  case (eval l env, eval r env) of
    (Right lv, Right rv) -> Right (lv - rv)
    (Left le, Left re) -> Left (le ++ "\n" ++ re)
    (x@(Left le), _) -> x
    (_, y@(Left le)) -> y
eval (Mul l r) env =
  case (eval l env, eval r env) of
    (Right lv, Right rv) -> Right (lv * rv)
    (Left le, Left re) -> Left (le ++ "\n" ++ re)
    (x@(Left le), _) -> x
    (_, y@(Left le)) -> y
eval (Div l r) env =
  case (eval l env, eval r env) of
    (Right _, Right 0) -> Left "Division by 0"
    (Right lv, Right rv) -> Right (lv `div` rv)
    (Left le, Left re) -> Left (le ++ "\n" ++ re)
    (x@(Left le), _) -> x
    (_, y@(Left le)) -> y

eval2 :: Expr -> Int
eval2 (Val n) = n
eval2 (Var n) = 0
eval2 (Add l r) = eval2 l + eval2 r
eval2 (Sub l r) = eval2 l - eval2 r
eval2 (Mul l r) = eval2 l * eval2 r
eval2 (Div l r) = eval2 l `div` eval2 r

value :: Int -> State (Stack Int) ()
value n = do
  st <- get
  put (stackPush st n)

binaryFunction :: (Int -> Int -> Int) -> Expr -> Expr -> State (Stack Int) ()
binaryFunction f a b = do
  eval3 a
  eval3 b
  st <- get
  let (sa, va) = fromJust (stackPop st)
  let (sb, vb) = fromJust (stackPop sa)
  put (stackPush sb (f va vb))

add :: Expr -> Expr -> State (Stack Int) ()
add a b = do
  binaryFunction (+) b a

sub :: Expr -> Expr -> State (Stack Int) ()
sub a b = do
  binaryFunction (-) b a

mul :: Expr -> Expr -> State (Stack Int) ()
mul a b = do
  binaryFunction (*) b a

div' :: Expr -> Expr -> State (Stack Int) ()
div' a b = do
  binaryFunction div b a

eval3 :: Expr -> State (Stack Int) ()
eval3 (Val v) = value v
eval3 (Add l r) = add l r
eval3 (Sub l r) = sub l r
eval3 (Mul l r) = mul l r
eval3 (Div l r) = div' l r
eval3 _ = value 0
