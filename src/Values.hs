module Values
  ( NumType,
    ValType,
    Name,
    defaultVal,
    toNumType,
    falseVal,
    trueVal,
    boolToVal,
    valToBool,
  )
where

type Name = String

type NumType = Int

type ValType = NumType

-- default value for ValType entities (such as variables)
defaultVal :: ValType
defaultVal = 0

toNumType :: String -> Either String NumType
toNumType num =
  case (read num) :: Integer of
    x | x > maxInt -> Left (num ++ " exceeds Int range!")
    x -> Right ((fromInteger x) :: Int)

maxInt = toInteger (maxBound :: Int)

falseVal, trueVal :: ValType
falseVal = 0
trueVal = 1

boolToVal :: Bool -> ValType
boolToVal True = trueVal
boolToVal False = falseVal

valToBool :: ValType -> Bool
valToBool x = (x /= falseVal)
