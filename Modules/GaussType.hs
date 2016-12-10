module GaussType where

import           Data.List
import           Helper
import           LinearEquation
import           NonlinearEquation
import           NumericalIntegration
import           Writer

fFunc :: ValueFunc
fFunc = sin

wFunc :: ValueFunc
wFunc x = 1 / (x + 0.1)

iSegment :: Segment
iSegment = (0, 1)

getMoments :: Segment -> Int -> [Value]
getMoments seg n = take (2*n) $ map (integrate . getMoment)  [0..] where
    getMoment k x = wFunc x * x ** k
    integrate func = integration func seg 100 Simpson

getMatrix :: [Value] -> Matrix
getMatrix moments = transpose $ map getRow [n - 1, n - 2 .. 0] ++ [map negate (getRow n)]  where
    myLen = length moments
    n = div myLen 2
    getRow index = take n . drop index $ moments

getCoefficient :: Matrix -> Row
getCoefficient = solve

getPolynomial :: [Value] -> ValueFunc
getPolynomial coeff = foldl1 sumFunc $ zipWith mulFunc (map const (1 : coeff)) monomials where
    monomial index x = x ** index
    myLen = toDouble . length $ coeff
    monomials = map monomial [myLen, myLen - 1.0 .. 0.0]

getNodes :: ValueFunc -> Segment -> [Value]
getNodes myEquation seg = map ((last . snd) . runWriter . bisection epsilon isPolynomialRoot)
    tabulated where
    segments = segmentation seg step
    isPolynomialRoot = isRoot' myEquation
    tabulated = filter isPolynomialRoot segments

getCoeffOfGauss :: [Value] -> [Value] -> [Value]
getCoeffOfGauss nodes moments = getCoefficient matrix where
    myLen = length nodes
    colMoments = take myLen moments
    matrix = transpose $ map (take myLen . (\ x -> iterate (\ y -> x * y) 1)) nodes ++ [colMoments]

getIntegral :: ValueFunc -> [Value] -> [Value] -> Value
getIntegral func nodes coeff = sum $ zipWith (*) (map func nodes) coeff

result :: ValueFunc -> Value
result func = integral where
    moments = getMoments iSegment 4
    matrix = getMatrix moments
    coeff = getCoefficient matrix
    polynomial = getPolynomial coeff
    nodes = getNodes polynomial iSegment
    coeffGauss = getCoeffOfGauss nodes moments
    integral = getIntegral func nodes coeffGauss

polynomialLegendre :: Int -> ValueFunc
polynomialLegendre 0 = const 1
polynomialLegendre 1 = id
polynomialLegendre n = \x -> pl2 x - pl1 x where
    pl1 y = m / (m + 1) * polynomialLegendre (mInt - 1) y
    pl2 y = (2 * m + 1) / (m + 1) * y * polynomialLegendre mInt y
    m = toDouble mInt
    mInt = n - 1

derivativePolynomialLegendre :: Int -> ValueFunc
derivativePolynomialLegendre nInt = \x -> n / (1 - x**2) * (polynomialLegendre (nInt - 1) x - x * polynomialLegendre nInt x) where
    n = toDouble nInt

getCoefficientLegendre :: Int -> ValueFunc
getCoefficientLegendre k x = 2 / ((1 - x**2) * derivative x ** 2) where
    derivative = derivativePolynomialLegendre k