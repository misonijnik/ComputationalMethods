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
          newtonToOutput
          newtonModifyToOutput


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
secantToOutput = do myPutStr "Метод Ньютона."
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
                                            myPutStr $ show $ abs (fst segNow - snd segNow)
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
                                myPutStr $ show $ abs (last arrVal - (last $ init arrVal))
                                putStrLn "Абсолютная величина невязки:"
                                myPutStr $ show $ abs (equation $ last arrVal)

secantHelper :: (Segment, [Value]) -> IO ()
secantHelper (seg, arrVal) = do putStrLn "Начальнай отрезок:"
                                myPutStr $ show seg
                                putStrLn "Начальное приближение:"
                                myPutStr $ show $ seg
                                putStrLn "Количество шагов для достижения точности эпсилон:"
                                myPutStr $ show $ length arrVal
                                putStrLn "Приближенное решение:"
                                myPutStr $ show $ last arrVal
                                putStrLn "Разность модулей последних приближений:"
                                myPutStr $ show $ abs (last arrVal - (last $ init arrVal))
                                putStrLn "Абсолютная величина невязки:"
                                myPutStr $ show $ abs (equation $ last arrVal)