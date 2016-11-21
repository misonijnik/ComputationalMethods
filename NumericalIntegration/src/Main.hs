module Main where

import NumericalIntegration

type Segment = (Double, Double)

main :: IO ()
main = do
    putStrLn "Приближённое вычисление интеграла по составным квадратурным формулам"
    putStrLn "Введите границы отрезка в форме: (a, b)"
    segment <- readASegment
    putStrLn "Введите число m"
    number <- readAInt
    let j = functionIntegral segment
    print $ "J = " ++ show j
    let integrationWith = integration function segment number
    putStrLn "Формула средних прямоугольников"
    let jh = integrationWith MediumRectangle
    print $ "J(h) = " ++ show jh
    print $ "|J - J(h)| = " ++ show (abs (j - jh))
    putStrLn "Формула трапеций"
    let jh = integrationWith Trapeze
    print $ "J(h) = " ++ show jh
    print $ "|J - J(h)| = " ++ show (abs (j - jh))
    putStrLn "Формула Симпсона"
    let jh = integrationWith Simpson
    print $ "J(h) = " ++ show jh
    print $ "|J - J(h)| = " ++ show (abs (j - jh))


readAInt :: IO Int
readAInt = readLn

readASegment :: IO Segment
readASegment = readLn
