import NonlinearEquation

main :: IO ()
main = do myPutStr "Численные методы решения нелинейных уравнений."
          myPutStr "Исходные параметры:"
          putStrLn "-интервал:"
          myPutStr (show lineSegment )
          putStrLn "- уравнение:"
          myPutStr "x ** 4 - 16 * x ** 3 + 500 * x ** 2 - 80000 * x + 32000"
          putStrLn "- эпсилон:"
          myPutStr (show epsilon) 
          tabSegToOutput
          bisecToOutput


myPutStr :: String -> IO ()
myPutStr s = do putStrLn s
                putStrLn ""

tabSegToOutput :: IO ()
tabSegToOutput = do putStrLn "Начальные отрезки:"
                    mapM_ print tabulatedSegments
                    putStrLn ""

bisecToOutput :: IO ()
bisecToOutput = do myPutStr "Метод половинного деления."
                   mapM_ bisecHelper bisectionMethods

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
                                            myPutStr $ show $ abs (fst segNow - snd segNow)
                                            putStrLn "Абсолютная величина невязки:"
                                            myPutStr $ show $ abs (equation $ last arrVal)