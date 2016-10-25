module Main where

import           FuncToTableHelper
import           Interpolation
import           ReverseInterpolation
import           Writer
import qualified NonlinearEquation as N
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
    putStrLn "Метод №1"
    let tableVal = map swap tableFoo
    let tableValSort = sortByMinDistance pointY tableVal
    putStrLn "Значение аргумента:"
    let polynomVal = lagrangePolynom tableValSort powerN
    print $ polynomVal pointY
    putStrLn "Модуль невязки"
    print $ abs (demoFunc (polynomVal pointY) - pointY)
    putStrLn "Метод №2"
    let segments = getListOfSeg tableFoo pointY
    let values = map (\(a, b) -> (a + b) / 2) segments
    let flipSort = flip sortByMinDistance
    let sortedTables = map (flipSort tableFoo) values
    let flipPolynom = flip lagrangePolynom
    let polynoms = map (flipPolynom powerN) sortedTables
    let listOfFunc = map (sumFunc  (const $ - pointY)) polynoms
    let eps = 10 ** (-8)
    let newBisection = megaFunc eps
    let segAndF = zip segments listOfFunc
    let arrVal = map newBisection segAndF
    putStrLn "Значения аргумента:"
    mapM_ print arrVal
    putStrLn "Модули невязки"
    let newFunc x = abs (sumFunc demoFunc (const (-pointY)) x)
    let arrMod = map newFunc arrVal
    mapM_ print arrMod

megaFunc :: Epsilon -> (Segment, ValueFunc) -> Value
megaFunc eps (seg, f) = last . snd $ runWriter $ N.bisection eps (N.isRoot' f) seg
