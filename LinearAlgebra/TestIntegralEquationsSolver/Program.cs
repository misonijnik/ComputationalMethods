using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Integral;
using Integral.IntegralEquations;

namespace TestIntegralEquationsSolver
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Численное решение интегрального уравнения Фредгольма 2-го рода");

            Func<double, double, double> k = (x, y) => Math.Log(3 + x*y)/3; //1 / (2 + x * y);
            Func<double, double> f = (x) => Math.Pow(1 - x,2); //Math.Pow(1 + x, 2); 
            double a;
            double b;
            GetSegment(out a, out b);
            int n = 10;
            int lPow1 = 4;
            int lPow2 = 6;
            Console.WriteLine("K(x,y) = ln(3 + x*y)/3");
            Console.WriteLine("F(x) = (1 - x) ^ 2");
            Console.WriteLine($"N = {n}");
            Console.WriteLine($"a = {a}, b = {b}");

            double h = (b - a) / n;
            
            Console.WriteLine("Метод замены на вырожденное ядро");
            //ряд ядра не сойдется
            if (b - a > 2)
            {
                Console.WriteLine("Возможен неточный ответ");
            }
            Func<double, double>[] alpha =
            {
                x => Math.Log(3),
                x => x / 3,
                x => -Math.Pow(x,2) / 6,
                x => Math.Pow(x,3) / 9
            };

            Func<double, double>[] beta =
            {
                y => 1.0/3,
                y => y/3,
                y => Math.Pow(y,2)/9,
                y => Math.Pow(y,3) / 27
            };
            int q1 = 3;
            int q2 = 4;

            var res3 = IntegralEquationSolver.KernelReplacementMethod(alpha, beta, f, a, b, q1);
            var res4 = IntegralEquationSolver.KernelReplacementMethod(alpha, beta, f, a, b, q2);

            //check
            double max = 0;
            for (int i = 0; i <= n; i++)
            {
                double x = a + h * i;
                max = Math.Max(max, Math.Abs(res4(x) - res3(x)));
            }
            Console.WriteLine($"Максимальная разница между решениями: {max}");

            Console.WriteLine("Метод механических квадратур");

            Console.WriteLine($"Степени многочлена Лежандра: {lPow1} и {lPow2}");
            var res1 = IntegralEquationSolver.MechanicalQuadratureMethod(k, f, a, b, lPow1);
            var res2 = IntegralEquationSolver.MechanicalQuadratureMethod(k, f, a, b, lPow2);

            //check
            max = 0;
            for (int i = 0; i <= n; i++)
            {
                double x = a + h * i;
                max = Math.Max(max, Math.Abs(res1(x) - res2(x)));
            }
            Console.WriteLine($"Максимальная разница между решениями: {max}\n");

            Console.WriteLine("ММК - 4:");
            for (int i = 0; i <= n; i++)
            {
                double x = a + h * i;
                Console.WriteLine(res1(x));
            }
            Console.WriteLine();
            Console.WriteLine("ММК - 6:");
            for (int i = 0; i <= n; i++)
            {
                double x = a + h * i;
                Console.WriteLine(res2(x));
            }
            Console.WriteLine();
            Console.WriteLine("Замена на вырожденное ядро порядка 3:");
            for (int i = 0; i <= n; i++)
            {
                double x = a + h * i;
                Console.WriteLine(res3(x));
            }
            Console.WriteLine();
            Console.WriteLine("Замена на вырожденное ядро порядка 4:");
            for (int i = 0; i <= n; i++)
            {
                double x = a + h * i;
                Console.WriteLine(res4(x));
            }

            Console.ReadKey();
        }

        public static void GetSegment(out double a, out double b)
        {
            do
            {
                Console.WriteLine("Введите A");
                if (!double.TryParse(Console.ReadLine(), out a))
                {
                    Console.WriteLine("Введено неверно\n");
                    continue;
                }
                break;
            } while (true);

            do
            {
                Console.WriteLine("Введите B");
                if (!double.TryParse(Console.ReadLine(), out b) || a >= b)
                {
                    Console.WriteLine("Введено неверно\n");
                    continue;
                }
                break;
            } while (true);
            Console.WriteLine();
        }
    }
}
