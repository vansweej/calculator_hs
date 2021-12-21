module EvalCalc
  ( ValType,
    Name,
    Expr (..),
    Env,
    EvalErr,
    eval,
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
