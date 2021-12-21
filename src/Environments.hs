module Environments
  ( AnEnv,
    Name,
    newEnv,
    toList,
    getBinding,
    hasBinding,
    newBinding,
    setBinding,
    bindList,
  )
where

import Values (Name)

type AnEnv a = [(Name, a)] -- generic environment type

newEnv :: AnEnv a
newEnv = []

toList :: AnEnv a -> [(Name, a)]
toList xs = xs

getBinding :: Name -> AnEnv a -> Maybe a
getBinding n env = lookup n env

hasBinding :: Name -> AnEnv a -> Bool
hasBinding n env =
  case getBinding n env of
    Just _ -> True
    Nothing -> False

newBinding :: Name -> a -> AnEnv a -> AnEnv a
newBinding n v env = (n, v) : env

setBinding :: Name -> a -> AnEnv a -> AnEnv a
setBinding n v env =
  let (ls, rs) = break (\(k, _) -> k == n) env
      trs =
        if null rs
          then error ("Attempt to set undefined variable " ++ n)
          else tail rs
   in ls ++ ((n, v) : trs)

bindList :: [(Name, a)] -> AnEnv a -> AnEnv a
bindList nvs env =
  foldl (\env1 (n, v) -> newBinding n v env1) env nvs
