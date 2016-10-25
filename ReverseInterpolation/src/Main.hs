module Main where

import           FuncToTableHelper
import           Interpolation
import           Data.Tuple

main :: IO ()
main = do
  putStrLn "Задача обратного интерполирования"
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

inputVal :: Segment -> Int -> IO ()
inputVal seg n = do
    putStrLn "Введите значение 'F'"
    pointY <- readADouble
    putStrLn $ "Введите степень интерполяционного многочлена меньше либо равную " ++ show n
    powerN <- readPower n
    let tableFoo = table seg n
    let tableVal = map swap tableFoo
    let tableSort = sortByMinDistance pointY tableVal
    putStrLn "Значение аргумента:"
    let polynomL = lagrangePolynom tableSort powerN
    print $ polynomL pointY
    putStrLn "Модуль невязки"
    print $ abs (demoFunc (polynomL pointY) - pointY)