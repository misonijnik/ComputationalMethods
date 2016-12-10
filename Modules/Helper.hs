module Helper where

type Value = Double
type Segment = (Double, Double)
type Epsilon = Double
type Node = (Value, Value)
type ValueFunc = (Value -> Value)

toDouble :: Int -> Double
toDouble x = fromInteger $ toInteger x

sumFunc :: ValueFunc -> ValueFunc -> ValueFunc
sumFunc a b x = a x + b x

mulFunc :: ValueFunc -> ValueFunc -> ValueFunc
mulFunc a b x = a x * b x