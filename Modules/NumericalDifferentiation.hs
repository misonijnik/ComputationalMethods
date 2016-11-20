module NumericalDifferentiation where

import FuncToTableHelper
import Data.List

data DiffType = Head | Middle Int | Tail

differentiationFromNodes :: [Node] -> [Value]
differentiationFromNodes nodes = map (differentiationByVal nodes h) arr
    where h = ((fst.last) nodes - (fst.head) nodes) / toDouble count
          count = length nodes - 1
          arr = [0 .. count]

differentiation2FromNodes :: [Node] -> [Value]
differentiation2FromNodes nodes = map (differentiation2ByVal nodes h) arr
    where h = ((fst . last) nodes - (fst . head) nodes) / toDouble count
          count = length nodes - 1
          arr = [0 .. count]

differentiationByVal :: [Node] -> Epsilon -> Int -> Value
differentiationByVal nodes eps index | index == 0  = diff Head
                                     | index == lt = diff Tail
                                     | otherwise   = diff (Middle index)
    where lt      = length nodes - 1
          diff tp = differentiation eps tp (getTriple nodes tp)

differentiation2ByVal :: [Node] -> Epsilon -> Int -> Value
differentiation2ByVal nodes eps index | index == 0  = diff Head
                                      | index == lt = diff Tail
                                      | otherwise   = diff (Middle index)
    where lt      = length nodes - 1
          diff tp = differentiation2 eps tp (getTriple nodes tp)

getTriple :: [Node] -> DiffType -> (Value, Value, Value)
getTriple nodes tp = case tp of
    Head         -> (values !! 0, values !! 1, values !! 2)
    Tail         -> (values !! (lt - 2), values !! (lt - 1), values !! lt)
    Middle index -> (values !! (index - 1), values !! index, values !! (index + 1))
    where lt     = length nodes - 1
          values = map snd nodes

differentiation :: Epsilon -> DiffType -> (Value, Value, Value) -> Value
differentiation h Head       (a, b, c) = (-3*a + 4*b - c)/(2*h) 
differentiation h Tail       (a, b, c) = (3*c - 4*b + a)/(2*h)
differentiation h (Middle _) (a, _, c) = (c - a)/(2*h)

differentiation2 :: Epsilon -> DiffType -> (Value, Value, Value) -> Value
differentiation2 _ Head       _         = 0 / 0
differentiation2 _ Tail       _         = 0 / 0
differentiation2 h (Middle _) (a, b, c) = (c - 2*b + a)/(h*h)

megaDifferentiation :: [Node] -> [[Value]]
megaDifferentiation nodes = (valX : valY : diff : diffR : diff2 : diff2R : [])
    where (valX, valY) = (map fst nodes, map snd nodes)
          diff = differentiationFromNodes nodes
          diffReal = map demoFuncD valX
          diffR = zipWith (\x y -> abs(x - y)) diff diffReal
          diff2 = differentiation2FromNodes nodes
          diff2Real = map demoFuncD2 valX
          diff2R = zipWith (\x y -> abs(x - y)) diff2 diff2Real