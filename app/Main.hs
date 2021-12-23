module Main where

import AbstractSyntax (Expr (..))
import Environments
  ( AnEnv,
    Name,
    bindList,
    getBinding,
    hasBinding,
    newBinding,
    newEnv,
    setBinding,
    toList,
  )

import Control.Monad.State
import Data.Stack
import EvalCalc (eval, eval2, eval3)
import Lib

main =
  do
    let env = [("x", 5), ("y", 7), ("z", 1)]
    let exp1 = Val 3 -- 3
    let exp2 = Var "x" -- x
    let exp3 = Add (Val 1) (Val 2) -- 1+2
    let exp4 = Add (Var "x") (Val 3) -- x + 3
    let exp5 =
          Mul
            (Add (Var "x") (Var "y"))
            (Add (Val 2) (Var "z")) -- (x + y) * (2 + z)
    putStrLn ("Expression: " ++ show exp1)
    putStrLn
      ( "Evaluation with x=5, y=7, z=1:  "
          ++ show (eval exp1 env)
      )
    putStrLn ("Expression: " ++ show exp2)
    putStrLn
      ( "Evaluation with x=5, y=7, z=1:  "
          ++ show (eval exp2 env)
      )
    putStrLn ("Expression: " ++ show exp3)
    putStrLn
      ( "Evaluation with x=5, y=7, z=1:  "
          ++ show (eval exp3 env)
      )
    putStrLn ("Expression: " ++ show exp4)
    putStrLn
      ( "Evaluation with x=5, y=7, z=1:  "
          ++ show (eval exp4 env)
      )
    putStrLn ("Expression: " ++ show exp5)
    putStrLn
      ( "Evaluation with x=5, y=7, z=1:  "
          ++ show (eval exp5 env)
      )

    let exp6 = Mul (Add (Val 2) (Val 5)) (Sub (Val 7) (Val 3))
    let exp7 = Add (Add (Val 3) (Val 1)) (Val 5)
    let startState = stackNew
    putStrLn ("eval3 evaluation: " ++ show (execState (eval3 exp6) startState))


