module InterpolationWithoutDivision where

import           Data.List         ()
import           FuncToTableHelper
import           Prelude

data InterOpt = Head | Middle Int | Tail

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
fDifferences (a : values) = (a' - a) : fDifferences values
    where a' = head values

finiteDifferences :: [Node] -> Int -> [[Value]]
finiteDifferences nodes n = values : finiteDifferences' values n
    where values = map snd nodes

finiteDifferences' :: [Value] -> Int -> [[Value]]
finiteDifferences' _ 0 = []
finiteDifferences' values n = nextDifferences : finiteDifferences' nextDifferences (n - 1)
    where nextDifferences = fDifferences values

newtonWDListFrom :: [Value] -> Int -> [ValueFunc]
newtonWDListFrom values n= const 1 : (reverse . newtonWDListFrom' $ cutValues)
    where cutValues = take n values

newtonWDListFrom' :: [Value]-> [ValueFunc]
newtonWDListFrom' [_] = []
newtonWDListFrom' values = omegaFrom (listMonomial initValues) : newtonWDListFrom' initValues
    where initValues = init values

getInterOpt :: [Node] -> Value -> InterOpt
getInterOpt nodes x
    | x < a     = Head
    | x > b     = Tail
    | otherwise = Middle (number - 1)
    where values = map fst nodes
          (a, b) = (head . tail $ values, last . init $ values)
          number = length . fst $ span (<= x) values

newtonPolynomWD :: [Node] -> Int -> ValueFunc
newtonPolynomWD nodes n x = case interOpt of
    Head         -> newtonPolynomWD' (newtonOmega indexMonomialHead) (map head finiteDiff) (x - a) / h
    Tail         -> newtonPolynomWD' (newtonOmega indexMonomialTail) (map last finiteDiff) (x - b) / h
    Middle count -> newtonPolynomWD' (newtonOmega indexMonomialMiddle) (zipWith (changeIndex count) finiteDiff indexFinite) (x - z count) / h
    where interOpt = getInterOpt nodes x
          newtonOmega indexMonomial = newtonWDListFrom indexMonomial n
          finiteDiff = finiteDifferences nodes n
          indexFinite = take (n + 1) indexFiniteWD
          (a, b) = (fst . head $ nodes, fst . last $ nodes)
          h = abs ((b - a) / toDouble (length nodes - 1))
          z index = fst $ nodes !! index

newtonPolynomWD' :: [ValueFunc] -> [Value] -> ValueFunc
newtonPolynomWD' newtonOmega finiteDiff = foldl1 sumFunc $ zipWith mulFunc newtonOmega (map const finiteDiff')
    where finiteDiff' = zipWith (/) finiteDiff (1.0 : scanl1 (*) [1.0 .. toDouble (length finiteDiff - 1)]) 

indexFiniteWD :: [Int]
indexFiniteWD = [- truncate (toDouble x / 2) | x <- [0 .. ]]

indexMonomialMiddle :: [Value]
indexMonomialMiddle = [(-1) ^ (x + 1) * toDouble (truncate (toDouble x / 2)) | x <- [0 .. ]]

indexMonomialHead :: [Value]
indexMonomialHead = [0 .. ]

indexMonomialTail :: [Value]
indexMonomialTail = [0, -1 .. ]
