module Main where

import NumericalDifferentiation
import FuncToTableHelper

main :: IO ()
main = do
  putStrLn "Формулы численного дифференциирования"
  putStrLn "Введите границы отрезка в форме: (a, b)"
  segment <- readASegment
  putStrLn "Введите число m"
  number <- readAInt
  let nodes = table segment number
  let tb = megaDifferentiation nodes
  let htable = ["Xi", "f(Xi)", "f'(Xi)nd", "|f'(Xi)t - f'(Xi)nd|", "f''(Xi)nd", "|f''(Xi)t - f''(Xi)nd|"]
  let bodytable = map (map show) tb
  let finaltb = map (map fixedSize) (htable : bodytable)
  mapM_ print finaltb

readASegment :: IO Segment
readASegment = readLn

readAInt :: IO Int
readAInt = readLn

fixedSize :: String -> String
fixedSize str = if len < 21 then str ++ getSpaces (myLen - len) else str
    where len = length str
          myLen = 21

getSpaces :: Int -> String
getSpaces n = replicate n ' '