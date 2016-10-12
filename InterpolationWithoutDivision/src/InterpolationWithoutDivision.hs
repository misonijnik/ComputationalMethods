module InterpolationWithoutDivision where

import           Prelude
import           Data.List
import           FuncToTableHelper

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

fDifferences :: [Value] -> [Value]
fDifferences [] = []
fDifferences [_] = []
fDifferences (a : values) = (a' -a) : fDifferences values
    where a' = head values

finiteDifferences' :: [Node] -> Int -> [[Value]]
finiteDifferences' nodes = finiteDifferences values
    where values = map snd nodes

finiteDifferences :: [Value] -> Int -> [[Value]]
finiteDifferences _ 0 = []
finiteDifferences values n = nextDifferences : finiteDifferences nextDifferences (n - 1)
    where nextDifferences = fDifferences values
{-
    newtonOmegaWDListFrom' :: [Node] -> [ValueFunc]
    newtonOmegaWDListFrom' listNode = reverse . newtonOmegaWDListFrom $ values
        where values = map fst listNode

    newtonOmegaWDListFrom :: [Value] -> [ValueFunc]
    newtonOmegaWDListFrom [_] = []
    newtonOmegaWDListFrom values = omegaFrom (listMonomial initValues) : newtonOmegaWDListFrom initValues
        where initValues = init values


    newtonPolynomWD :: [Node] -> Int -> ValueFunc
    newtonPolynomWD listNode n = sumFunc (const $ snd.head $ cutListNode) (foldl1 sumFunc listTmpPolynom)
        where cutListNode = take (n+1) listNode
            finiteDiff = finiteDifferences' cutListNode n
            finiteDiff' = map head finiteDiff
            newtonOmegaWD = newtonOmegaWDListFrom' cutListNode
            listTmpPolynom = zipWith mulFunc newtonOmegaWD (map const finiteDiff')
-}

indexFiniteWD :: [Int]
indexFiniteWD = [- truncate (toDouble x / 2) | x <- [0 .. ]]

indexMonomialWD :: [Int]
indexMonomialWD = [(-1) ^ x * truncate (toDouble x / 2) | x <- [0 .. ]]