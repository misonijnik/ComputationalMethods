module Main where

import           FuncToTableHelper
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Interpolation

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
    putStrLn "Введите точку 'x'"
    pointX <- readADouble
    putStrLn $ "Введите степень интерполяционного многочлена меньше либо равную " ++ show n
    powerN <- readPower n
    let tableFoo = table seg n
    let tableSort = sortByMinDistance pointX tableFoo
    putStrLn "Отсортированная таблица:"
    mapM_ print tableSort
    putStrLn $ "Значение интерполяционного многочлена в форме Ньютона в точке " ++ show pointX
    let polynomN = newtonPolynom tableSort powerN
    print $ polynomN pointX
    putStrLn $ "Разница значений функции и интерполяционного многочлена в форме Ньютона в точке " ++ show pointX
    print $ abs (polynomN pointX - demoFunc pointX)
    putStrLn $ "Значение интерполяционного многочлена в форме Лагранжа в точке " ++ show pointX
    let polynomL = lagrangePolynom tableSort powerN
    print $ polynomL pointX
    putStrLn $ "Разница значений функции и интерполяционного многочлена в форме Лагранжа в точке " ++ show pointX
    print $ abs (polynomL pointX - demoFunc pointX)
    graph polynomL

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
