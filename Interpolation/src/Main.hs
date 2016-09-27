module Main where

import Interpolation
import FuncToTableHelper
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = do
  putStrLn "Задача алгебраического интерполирования"
  putStrLn "Интерполяционный многочлен в форме Лагранжа"
  putStrLn "Функция в табличной форме:"
  mapM_ print table
  nextWork
  
readAInt :: IO Int
readAInt = readLn

readADouble :: IO Double
readADouble = readLn

nextWork :: IO ()
nextWork = do 
    inputVal
    putStrLn "Введите 'exit', если хотите закончить работу. Иначе нажмите 'Enter'"
    str <- getLine
    if str == "exit"
        then do 
            return ()
        else do
            nextWork

readPower :: IO Int
readPower = do 
    powerN <- readAInt
    if ((powerN > numberOfValues) || (powerN < 0))
        then do 
            putStrLn "Ошибка. Введите верную степень"
            powerN <- readPower
            return powerN
        else do
            return powerN

inputVal :: IO ()
inputVal = do
    putStrLn "Введите точку 'x'"
    pointX <- readADouble
    putStrLn $ "Введите степень интерполяционного многочлена меньше либо равную " ++ (show numberOfValues)
    powerN <- readPower
    putStrLn $ "Значение интерполяционного многочлена в точке " ++ (show pointX)
    let polynom = lagrangePolynom table powerN
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
