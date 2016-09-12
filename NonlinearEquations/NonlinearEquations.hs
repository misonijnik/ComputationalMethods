module NonlinearEquation where

import           Prelude
import           Writer

type Segment = (Double, Double)
data Methods = Bisection | Newton deriving (Eq, Show)
type Epsilon = Double
type Value   = Double 

equation :: Value -> Value
--equation x = 25552 - 30 * x ** 2 + x ** 3

diffEquation :: Value -> Value
--diffEquation x = -60 * x + 3 * x ** 2

equation x = x ** 4 - 16 * x ** 3 + 500 * x ** 2 - 80000 * x + 32000
diffEquation x = 4 * x ** 3 - 48 * x ** 2 + 1000 * x - 80000  

lineSegment :: Segment
lineSegment = (-10000, 10000)

step :: Epsilon
step = 0.05

epsilon :: Epsilon
epsilon = 0.001

isRoot :: Segment -> Bool
isRoot (x, y) = (equation x * equation y) <= 0

segmentation :: Segment -> Epsilon -> [Segment]
segmentation (a, b) h = zip x y
    where x = [a, a+h .. b - h]
          y = tail x ++ [b]

initialApproximation :: Segment -> Value
initialApproximation (a, b) = (a + b) / 2

tabulation :: [Segment] -> [Segment]
tabulation = filter isRoot

tabulatedSegments :: [Segment]
tabulatedSegments = tabulation $ segmentation lineSegment step

-- (эпсилон, сегмент)
bisection :: Epsilon -> Segment -> Writer [Value] Segment
bisection eps (a,b)
    | abs (a - b) <= 2 * eps = Writer ((a,b), [c])
    | isRoot (a, c)          = Writer ((a,c), [c]) >>= bisection eps
    | isRoot (c, b)          = Writer ((c,b), [c]) >>= bisection eps
    where c = (a + b)/2

newtonApproximation ::  Value -> Value
newtonApproximation x = x - equation x/diffEquation x

newtonApproximationModify :: (Value, Value) -> Value
newtonApproximationModify (x, y) = x - equation x / y

secantApproximation :: (Value, Value) -> (Value, Value)
secantApproximation (x, y) = (y, y - equation y * (y - x) / (equation y - equation x))

secant :: Epsilon -> Segment -> [Double]
secant eps x@(a, b)
    | abs (a - b) <= eps = [b]
    | otherwise          = snd pair : secant eps pair
    where pair =  secantApproximation x

newtonModify :: Value -> Epsilon -> Value -> [Value]
newtonModify diffVal eps x
    | abs (x - xk) <= eps = [xk]
    | otherwise          = xk : newtonModify diffVal eps xk
    where xk = newtonApproximationModify (x, diffVal)

-- (эпсилон, x к-тое)
newton :: Epsilon -> Value -> [Value]
newton eps x
    | abs (x - xk) <= eps = [xk]
    | otherwise          = xk : newton eps xk
    where xk = newtonApproximation x

bisectionMethods :: [(Segment, [Value])]
bisectionMethods = map (runWriter . bisection epsilon) tabulatedSegments

newtonMethods :: [[Value]]
newtonMethods = map (newton epsilon . initialApproximation) tabulatedSegments

newtonModifyMethods :: [[Value]]
newtonModifyMethods = map (\val -> newtonModify (diffEquation . initialApproximation $ val)  epsilon (initialApproximation val)) tabulatedSegments

secantMethods :: [[Double]]
secantMethods = map (secant epsilon) tabulatedSegments