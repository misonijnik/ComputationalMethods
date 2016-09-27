module Main where

import Interpolation
import FuncToTableHelper
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = do
  putStrLn "Задача алгебраического интерполирования"
  putStrLn "Интерполяционный многочлен в форме Лагранжа"
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
        then do 
            return ()
        else do
            nextWork seg n

readPower :: Int -> IO Int
readPower n = do 
    powerN <- readAInt
    if ((powerN > n) || (powerN < 0))
        then do 
            putStrLn "Ошибка. Введите верную степень"
            powerN <- readPower n
            return powerN
        else do
            return powerN

inputVal :: Segment -> Int -> IO ()
inputVal seg n = do
    putStrLn "Введите точку 'x'"
    pointX <- readADouble
    putStrLn $ "Введите степень интерполяционного многочлена меньше либо равную " ++ (show n)
    powerN <- readPower n
    let tableFoo = table seg n
    let tableSort = sortByMinDistance pointX tableFoo
    putStrLn "Отсортированная таблица:"
    mapM_ print tableSort
    putStrLn $ "Значение интерполяционного многочлена в точке " ++ (show pointX)
    let polynom = lagrangePolynom tableSort powerN
    print $ polynom pointX
    putStrLn $ "Разница значений функции и интерполяционного многочлена в точке " ++ (show pointX)
    print $ abs (polynom pointX - demoFunc pointX)
    graph polynom

graph :: ValueFunc -> IO ()
graph func = toFile def "graph.png" $ do
    layout_title .= "Графики функии и многочлена"
    setColors [opaque blue, opaque red]
    plot (line "Функция" [signal demoFunc listPoint])
    plot (line "Многочлен" [signal func listPoint])


signal :: ValueFunc -> [Value] -> [Node]
signal func xs = [ (x, func x) | x <- xs ]

listPoint :: [Value]
listPoint = [-1,(-0.999)..1.5]
