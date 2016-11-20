import           NonlinearEquation

main :: IO ()
main = do myPutStr "Численные методы решения нелинейных уравнений."
          myPutStr "Исходные параметры:"
          putStrLn "-интервал:"
          myPutStr (show lineSegment )
          putStrLn "- уравнение:"
          myPutStr "(2 - x)* exp x - 0.5"
          putStrLn "- эпсилон:"
          myPutStr (show epsilon)
          tabSegToOutput
          bisecToOutput
          newtonToOutput
          newtonModifyToOutput
          secantToOutput
          getVal


myPutStr :: String -> IO ()
myPutStr s = do putStrLn s
                putStrLn ""

tabSegToOutput :: IO ()
tabSegToOutput = do putStrLn "Начальные отрезки:"
                    mapM_ print tabulatedSegments
                    putStrLn ""

bisecToOutput :: IO ()
bisecToOutput = do myPutStr "Метод половинного деления."
                   mapM_ bisecHelper bisectionMethod

newtonToOutput :: IO ()
newtonToOutput = do myPutStr "Метод Ньютона."
                    mapM_ newtonHelper newtonMethod

newtonModifyToOutput :: IO ()
newtonModifyToOutput = do myPutStr "Модифицированный метод Ньютона"
                          mapM_ newtonHelper newtonModifyMethod

secantToOutput :: IO ()
secantToOutput = do myPutStr "Метод секущих."
                    mapM_ secantHelper secantMethod

bisecHelper :: (Segment, (Segment, [Value])) -> IO ()
bisecHelper (segOld, (segNow, arrVal)) = do putStrLn "Начальнай отрезок:"
                                            myPutStr $ show segOld
                                            putStrLn "Начальное приближение:"
                                            myPutStr $ show $ initialApproximation segOld
                                            putStrLn "Количество шагов для достижения точности эпсилон:"
                                            myPutStr $ show $ length arrVal
                                            putStrLn "Приближенное решение:"
                                            myPutStr $ show $ last arrVal
                                            putStrLn "Разность модулей последних приближений:"
                                            myPutStr $ show $ abs (uncurry (-) segNow)
                                            putStrLn "Абсолютная величина невязки:"
                                            myPutStr $ show $ abs (equation $ last arrVal)

newtonHelper :: (Segment, [Value]) -> IO ()
newtonHelper (seg, arrVal) = do putStrLn "Начальнай отрезок:"
                                myPutStr $ show seg
                                putStrLn "Начальное приближение:"
                                myPutStr $ show $ initialApproximation seg
                                putStrLn "Количество шагов для достижения точности эпсилон:"
                                myPutStr $ show $ length arrVal
                                putStrLn "Приближенное решение:"
                                myPutStr $ show $ last arrVal
                                putStrLn "Разность модулей последних приближений:"
                                myPutStr $ show $ abs (last arrVal - last (init arrVal))
                                putStrLn "Абсолютная величина невязки:"
                                myPutStr $ show $ abs (equation $ last arrVal)

secantHelper :: (Segment, [Value]) -> IO ()
secantHelper (seg, arrVal) = do putStrLn "Начальнай отрезок:"
                                myPutStr $ show seg
                                putStrLn "Начальное приближение:"
                                myPutStr $ show seg
                                putStrLn "Количество шагов для достижения точности эпсилон:"
                                myPutStr $ show $ length arrVal
                                putStrLn "Приближенное решение:"
                                myPutStr $ show $ last arrVal
                                putStrLn "Разность модулей последних приближений:"
                                myPutStr $ show $ abs (last arrVal - last (init arrVal))
                                putStrLn "Абсолютная величина невязки:"
                                myPutStr $ show $ abs (equation $ last arrVal)


getVal :: IO ()
getVal = do putStr "Метод половинного деления."
            print $ map bisecVal bisectionMethod
            putStrLn "Абсолютная величина невязки:"
            myPutStr $ show $ abs $ equation $ last $ map bisecVal bisectionMethod
            --myPutStr $ show $ map (len . bisecVal) bisectionMethod

            putStr "Метод Ньютона."
            print $ map anotherVal newtonMethod
            putStrLn "Абсолютная величина невязки:"
            myPutStr $ show $ abs $ equation $ last $ map anotherVal newtonMethod
            --myPutStr $ show $ map (len . anotherVal) newtonMethod

            putStr "Модифицированный метод Ньютона"
            print $ map anotherVal newtonModifyMethod
            putStrLn "Абсолютная величина невязки:"
            myPutStr $ show $ abs $ equation $ last $ map anotherVal newtonModifyMethod
            -- myPutStr $ show $ map (len.anotherVal) newtonModifyMethod

            putStr "Метод секущих."
            print $ map anotherVal secantMethod
            putStrLn "Абсолютная величина невязки:"
            myPutStr $ show $ abs $ equation $ last  $ map anotherVal secantMethod
            --myPutStr $ show $ map (len.anotherVal) secantMethod