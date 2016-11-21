module Main where

import           FuncToTableHelper
import           InterpolationWithoutDivision

main :: IO ()
main = do
    putStrLn "Задача интерполирования по равноотстоящим узлам"
    putStrLn "Введите границы отрезка в форме: (a, b)"
    segment <- readASegment
    putStrLn "Введите число m"
    number <- readAInt
    putStrLn "Функция в табличной форме:"
    let tableFoo = table segment number
    mapM_ print tableFoo
    nextWork segment number

readAInt :: IO Int
readAInt = readLn

readADouble :: IO Double
readADouble = readLn

readAValue :: (Double -> Bool) -> IO Double
readAValue predicate = do
    val <- readADouble
    if predicate val
        then return val
        else do
            putStrLn "Ошибка. Введите верное значение"
            readAValue predicate


readASegment :: IO Segment
readASegment = readLn

nextWork :: Segment -> Int-> IO ()
nextWork seg n = do
    inputVal seg n
    putStrLn "Введите 'exit', если хотите закончить работу. Иначе нажмите 'Enter'"
    str <- getLine
    if str == "exit"
        then return ()
        else nextWork seg n

readPower :: Int -> IO Int
readPower n = do
    powerN <- readAInt
    if (powerN > n) || (powerN < 0)
        then do
            putStrLn "Ошибка. Введите верную степень"
            readPower n
        else return powerN

isInDiap :: [Segment] -> Value -> Bool
isInDiap listSeg x =  any predicate listSeg
    where predicate (a, b) = a <= x && b >= x

inputVal :: Segment -> Int -> IO ()
inputVal seg n = do
    putStrLn $ "Введите степень интерполяционного многочлена меньше либо равную " ++ show n
    powerN <- readPower n
    let h = (snd seg - fst seg) / toDouble n
    let tmp = h * toDouble (floor $ (toDouble powerN + 1.0) / 2.0)
    let listSeg = [(fst seg, fst seg + h), (fst seg + tmp, snd seg - tmp), (snd seg - h, snd seg)]
    putStrLn "Введите точку 'x' из промежутков"
    mapM_ print listSeg
    pointX <- readAValue $ isInDiap listSeg
    let tableFoo = table seg n
    putStrLn $ "Значение интерполяционного многочлена в точке " ++ show pointX
    let polynom = newtonPolynomWD tableFoo powerN
    print $ polynom pointX
    putStrLn $ "Разница значений функции и интерполяционного многочлена в точке " ++ show pointX
    print $ abs (polynom pointX - demoFunc pointX)
