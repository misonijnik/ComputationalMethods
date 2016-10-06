module FuncToTableHelper where

import           Prelude

demoFunc :: ValueFunc
--demoFunc x = 1/(1 + 25*x**2)
--demoFunc x = 1 - exp(-x) + x ** 2
demoFunc x = x ** 2

type Value = Double
type Segment = (Double, Double)
type Epsilon = Double
type Node = (Value, Value)
type ValueFunc = (Value -> Value)

toDouble :: Int -> Double
toDouble x = fromInteger $ toInteger x

divisionIntoUnits :: Int -> Segment -> [Value]
divisionIntoUnits n (a, b) = [a + h * toDouble j | j <- [0..n]]
    where h = (b - a) / toDouble n

funcToTable :: ValueFunc -> [Value] -> [Node]
funcToTable func arr = zip arr y
    where y = map func arr

deleteByIndex :: [a] -> Int -> [a]
deleteByIndex xs n = ys ++ tail zs
    where (ys, zs) = splitAt n xs


table :: Segment -> Int -> [Node]
table seg n = funcToTable demoFunc (divisionIntoUnits n seg)
