module NonlinearEquation where

import           Prelude

equation :: Double -> Double
-- equation x = 25552 - 30 * x ** 2 + x ** 3
equation x = x ** 4 - 16 * x ** 3 + 500 * x ** 2 - 80000 * x + 32000

type Segment = (Double, Double)

data Methods = Bisec deriving (Eq, Show)

lineSegment :: Segment
lineSegment = (-1000, 1000)

step :: Double
step = 0.01

epsilon :: Double
epsilon = 0.001

isRoot :: Double -> Double -> Bool
isRoot x y = (equation x * equation y) <= 0

tabulation :: Segment -> Double -> [Segment]
tabulation (a, b) h
    | abs (a - b) < h = []
    | isRoot a c = (a, c) : tabulation (c, b) h
    | otherwise = tabulation (c, b) h
    where c = a + h

bisection :: Double -> Segment -> Writer [Double] Double 
bisection eps (a,b)
    | abs (a - b) <= 2 * eps = c
    | isRoot a c = bisection eps (a, c)
    | isRoot c b = bisection eps (c, b)
    where c = (a + b)/2



result :: Methods -> [Double]
result typeM
    | typeM == Bisec = map (bisection epsilon) $ tabulation lineSegment step