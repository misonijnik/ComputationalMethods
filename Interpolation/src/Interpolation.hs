module Interpolation where

import FuncToTableHelper
import Data.List

distanceFrom :: Value -> Node -> Node -> Ordering
distanceFrom x (y, _) (z, _)
    | abs(x - y) < abs(x - z) = LT
    | abs(x - y) > abs(x - z) = GT
    | otherwise               = EQ

sortByMinDistance :: Value -> [Node] -> [Node]
sortByMinDistance x = sortBy (distanceFrom x)

monomialFrom :: Value -> ValueFunc
monomialFrom xj = \x -> x - xj

sumFunc :: ValueFunc -> ValueFunc -> ValueFunc
sumFunc a b = \x -> a x + b x

mulFunc :: ValueFunc -> ValueFunc -> ValueFunc
mulFunc a b = \x -> a x * b x

listMonomial :: [Node] -> [ValueFunc]
listMonomial nodes = map (monomialFrom . fst) nodes

omegaFrom :: [ValueFunc] -> ValueFunc
omegaFrom = foldl1 mulFunc

omegaWithoutK :: Int -> [ValueFunc] -> ValueFunc
omegaWithoutK k listFunc = omegaFrom $ deleteByIndex listFunc k

lagrangePolynomK :: [ValueFunc] -> Int -> ValueFunc
lagrangePolynomK listFunc k = \x -> (omegaWK x)/(omegaWK kMonomValue)
    where kMonomValue  = negate $ (listFunc !! k) 0
          omegaWK = omegaWithoutK k listFunc

