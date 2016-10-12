module Interpolation where

import           Data.List
import           FuncToTableHelper

distanceFrom :: Value -> Node -> Node -> Ordering
distanceFrom x (y, _) (z, _)
    | abs(x - y) < abs(x - z) = LT
    | abs(x - y) > abs(x - z) = GT
    | otherwise               = EQ

sortByMinDistance :: Value -> [Node] -> [Node]
sortByMinDistance x = sortBy (distanceFrom x)

monomialFrom :: Value -> ValueFunc
monomialFrom xj x = x - xj

sumFunc :: ValueFunc -> ValueFunc -> ValueFunc
sumFunc a b x = a x + b x

mulFunc :: ValueFunc -> ValueFunc -> ValueFunc
mulFunc a b x = a x * b x

listMonomial' :: [Node] -> [ValueFunc]
listMonomial' nodeList = listMonomial $ map fst nodeList

listMonomial :: [Value] -> [ValueFunc]
listMonomial = map monomialFrom

omegaFrom :: [ValueFunc] -> ValueFunc
omegaFrom = foldl1 mulFunc

omegaWithoutK :: Int -> [ValueFunc] -> ValueFunc
omegaWithoutK k listFunc = omegaFrom $ deleteByIndex listFunc k

lagrangePolynomK :: [ValueFunc] -> Int -> ValueFunc
lagrangePolynomK listFunc k x = omegaWK x / omegaWK kMonomValue
    where kMonomValue  = negate $ (listFunc !! k) 0
          omegaWK = omegaWithoutK k listFunc

lagrangePolynom :: [Node] -> Int -> ValueFunc
lagrangePolynom listNode n = foldl1 sumFunc listTmpPolynom
    where listNum     = [0..n]
          cutListNode = take (n+1) listNode
          listLagrangePolynomK = map (lagrangePolynomK (listMonomial' cutListNode)) listNum
          listTmpPolynom = zipWith mulFunc (map (const . snd) cutListNode) listLagrangePolynomK

sDifferences :: [(Node, Value)] -> [(Node, Value)]
sDifferences [] = []
sDifferences [_] = []
sDifferences (a : values) = ((firstX, lastX),(lastY - firstY)/(lastX - firstX)) : sDifferences values
    where ((firstX, _), firstY) = a
          ((_, lastX), lastY) = head values

splitDifferences' :: [Node] -> Int -> [[Value]]
splitDifferences' values n = map (map snd) (splitDifferences nodesV n)
    where nodesV = map f values
          f (a, b) = ((a, a), b)

splitDifferences :: [(Node, Value)] -> Int -> [[(Node, Value)]]
splitDifferences _ 0 = []
splitDifferences values n = nextDifferences : splitDifferences nextDifferences (n - 1)
    where nextDifferences = sDifferences values

newtonOmegaListFrom' :: [Node] -> [ValueFunc]
newtonOmegaListFrom' listNode = reverse . newtonOmegaListFrom $ values
    where values = map fst listNode

newtonOmegaListFrom :: [Value] -> [ValueFunc]
newtonOmegaListFrom [_] = []
newtonOmegaListFrom values = omegaFrom (listMonomial initValues) : newtonOmegaListFrom initValues
    where initValues = init values

newtonPolynom :: [Node] -> Int -> ValueFunc
newtonPolynom listNode n = sumFunc (const $ snd.head $ cutListNode) (foldl1 sumFunc listTmpPolynom)
    where cutListNode = take (n+1) listNode
          finiteDiff = splitDifferences' cutListNode n
          finiteDiff' = map head finiteDiff
          newtonOmega = newtonOmegaListFrom' cutListNode
          listTmpPolynom = zipWith mulFunc newtonOmega (map const finiteDiff')
