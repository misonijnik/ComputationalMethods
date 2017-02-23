module Main where

import           GaussType
import           Helper

main :: IO ()
main = do
    putStrLn "Приблеженное вычисление интегралов при помощи квадратурных формул Наивысшей Алгебраической Степени Точности (КФ НАСТ)\n"
    putStrLn "Введите число N"
    n <- readInt
    putStrLn "Введите отрезок интегрирования"
    seg <- readSegment
    putStrLn $ "\nКФ типа Гаусса с " ++ show n ++ " узлами:"
    let moments = getMoments seg n
    putStrLn "\nМоменты весовой функции"
    print moments
    let matrix = getMatrix moments
    let coeff = getCoefficient matrix
    putStrLn "\nОртогональный многочлен"
    print (showPolynomial (1 : coeff))
    let polynomial = getPolynomial coeff
    let nodes = getNodes polynomial seg
    putStrLn "\nУзлы"
    print nodes
    let coeffGauss = getCoeffOfGauss nodes moments
    putStrLn "\nКоэффициенты"
    print coeffGauss
    let integral = getIntegral fFunc nodes coeffGauss
    putStrLn $ "\nJ = " ++ show integral
    putStrLn $ "\nКФ Гаусса с " ++ show n ++ " узлами:"
    let pl = polynomialLegendre n
    let nodesL = getNodes pl (-1, 1)
    putStrLn "\nУзлы"
    print nodesL
    let coeffGaussL = getCoefficientLegendre nodesL
    putStrLn "\nКоэффициенты"
    print coeffGaussL
    let integralL = getIntegralLegendre psiFunc seg nodesL coeffGaussL
    putStrLn $ "\nJ = " ++ show integralL

showPolynomial :: [Value] -> String
showPolynomial = getStrPolynomial where
    getStrPolynomial pol | len == 0  = []
                         | len == 1  = "(" ++ show x ++ ")"
                         | otherwise = "(" ++ show x ++ ")" ++ "x^" ++ show power ++ " + " ++ getStrPolynomial nextPol
        where
            len = length pol
            power = length nextPol
            (x : nextPol) = pol

readInt :: IO Int
readInt = readLn

readSegment :: IO Segment
readSegment = readLn
