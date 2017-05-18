using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using HeatEquation;

namespace TestHeatEquation
{
    class Program
    {
        static void Main(string[] args)
        {
            Func<double, double, double> uSol = (x, t) => Math.Exp(-0.25 * t) * (Math.Sin(0.5*x) + 1 - x);
            Func<double, double, double> f = (x, t) => -0.25*Math.Exp(-0.25*t) * (1 - x);
            Func<double, double> g = x => uSol(x, 0);
            Func<double, double> alpha = t => uSol(0, t);
            Func<double, double> betta = t => uSol(1, t);

            Console.WriteLine("f(x,t) = -0.25 * e^(-0.25 * t) * (1 - x)");
            Console.WriteLine("g(x) = sin(0.5 * x) + 1 - x");
            Console.WriteLine("alpha(t) = e^(-0.25 * t) ");
            Console.WriteLine("betta(t) = e^(-0.25 * t) * sin(0.5)");

            int n = GetN();
            double tmpTau = (1.0 / (2* n * n));
            double tMax = GetT();
            int m = (int)Math.Round(tMax / tmpTau) + 1;
            Console.WriteLine($"h = {1.0/n}");
            double h = 1.0d / n;
            double tau = tMax / m;
            Console.WriteLine($"m = {m}");
            //Console.WriteLine($"tau = {tau}");
            var res = HeatEquationSolver.HeatEquation1(tMax, 1, n, m, f, g, alpha, betta);
            var res2 = HeatEquationSolver.HeatEquation2(tMax, 1, n, m, f, g, alpha, betta);
            double max = 0;


            while (true)
            {
                max = 0;
                int q = GetK();

                for (int i = 0; i <= n; i++)
                {
                    double xi = i * h;
                    for (int k = 0; k <= m; k++)
                    {
                        double tk = k * tau;
                        //max = Math.Max(max, Math.Abs(res[i, k] - uSol(xi, tk)));

                        if (q == k)
                        {
                            max = Math.Max(max, Math.Abs(res[i, k] - uSol(xi, tk)));
                            Console.WriteLine($"U_{i}_{k} = {res[i, k]}");
                        }
                    }
                }
                Console.WriteLine($"|u* - u| = {max}\n");

                max = 0;
                for (int i = 0; i <= n; i++)
                {
                    double xi = i * h;
                    for (int k = 0; k <= m; k++)
                    {
                        double tk = k * tau;
                        //max = Math.Max(max, Math.Abs(res2[i, k] - uSol(xi, tk)));
                        if (q == k)
                        {
                            max = Math.Max(max, Math.Abs(res2[i, k] - uSol(xi, tk)));
                            Console.WriteLine($"U_{i}_{k} = {res[i, k]}");
                        }
                    }
                }
                Console.WriteLine($"|u* - u| = {max}\n");
            }
        }

        public static int GetN()
        {
            while (true)
            {
                Console.WriteLine("Введите N");
                string value = Console.ReadLine();
                int result;

                if (int.TryParse(value, out result) && result > (0))
                {
                    return result;
                }
                Console.WriteLine("Введено неверно");
            }
        }

        public static int GetK()
        {
            while (true)
            {
                Console.WriteLine("Введите K");
                string value = Console.ReadLine();
                int result;

                if (int.TryParse(value, out result) && result > (0))
                {
                    return result;
                }
                Console.WriteLine("Введено неверно");
            }
        }

        public static int GetM()
        {
            while (true)
            {
                Console.WriteLine("Введите M");
                string value = Console.ReadLine();
                int result;

                if (int.TryParse(value, out result) && result > (0))
                {
                    return result;
                }
                Console.WriteLine("Введено неверно");
            }
        }

        public static double GetT()
        {
            while (true)
            {
                Console.WriteLine("Введите T");
                string value = Console.ReadLine();
                double result;

                if (double.TryParse(value, out result) && result > (0))
                {
                    return result;
                }
                Console.WriteLine("Введено неверно");
            }
        }

        public static double GetH()
        {
            while (true)
            {
                Console.WriteLine("Введите h");
                string value = Console.ReadLine();
                double result;

                if (double.TryParse(value, out result) && result > 0)
                {
                    return result;
                }
            }
        }
    }
}
