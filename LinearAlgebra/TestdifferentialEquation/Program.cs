using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SystemLinearEquation;
using DifferentialEquationSolver;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace TestdifferentialEquation
{
    class Program
    {

        public static void Main(string[] args)
        {
            Matrix m = Matrix.Create(new Double[,]
            {
                {2, -1, 0, 0, 0},
                {-3, 8, -1, 0, 0},
                {0, 5, 12, 2, 0},
                {0, 0, -6, 18, -4},
                {0, 0, 0, -5, 10}
            });
            Vector d = Vector.Create(new Double[]
            {
                -25, 72, -69, -156, 20
            });
            Console.WriteLine(m);
            Console.WriteLine(d);
            var x = Solver.TridiagonalMethod(m, d);
            Console.WriteLine($"Решение:\n{x}");
            Console.WriteLine($"Норма вектора невязки:\n{Vector.Substraction(Matrix.MultiplyOnVector(m,x),d).Norm(Vector.NumP.Infinity)}");

            Console.WriteLine();
            Console.WriteLine("********************************************************");
            Console.WriteLine("y'' + ln(x+2) * y' - (1 + x) * y = e^(x/2)");
            Console.WriteLine("y'(0) = 0.6 * y(0)");
            Console.WriteLine("y'(1) = -0.3 * y(1)");

            var a = 0.0;
            var b = 1.0;
            Func<double, double> p = t => 1;
            Func<double, double> q = t => 1 + Math.Log(t + 2);
            Func<double, double> r = t => -1 - t;
            Func<double, double> f = t => Math.Exp(t / 2);
            var al0 = 0.6;
            var al1 = -1;
            var bt0 = 0.3;
            var bt1 = 1;
            var A = 0;
            var B = 0;
            var n = 10;
            Console.WriteLine($"n = {n}");
            Console.WriteLine();
            Console.WriteLine("Аппроксимация производной на границе первого порядка:");
            Vector result = DifferentialEquation.DifferenceMethod1(
                p, q, r, f, a, b, n, al0, al1, A, bt0, bt1, B);
            Console.WriteLine(result);
            Console.WriteLine("Максимальная погрешность при подстановки приближенных значений в уравнение:");
            //check
            double h = (b - a) / n;
            double max = 0;
            for (int i = 2; i <= n; i++)
            {
                double res = a + i * h;
                var y2 = (result[i + 1] - 2 * result[i] + result[i - 1]) / (h * h);
                var y1 = (result[i + 1] - result[i - 1]) / (2 * h);
                var y = result[i];
                max = Math.Max(max,
                    Math.Abs(y2 * p(res) + y1 * q(res) + y * r(res) - f(res)));
            }
            Console.WriteLine(max);
            Console.WriteLine();
            Console.WriteLine("Аппроксимация производной на границе второго порядка:");
            result = DifferentialEquation.DifferenceMethod2(
                p, q, r, f, a, b, n, al0, al1, A, bt0, bt1, B);
            Console.WriteLine();
            Console.WriteLine(result);
            Console.WriteLine("Максимальная погрешность при подстановки приближенных значений в уравнение:");
            //check
            h = (b - a) / n;
            max = 0;
            for (int i = 2; i <= n; i++)
            {
                double res = a + i * h;
                var y2 = (result[i + 1] - 2 * result[i] + result[i - 1]) / (h * h);
                var y1 = (result[i + 1] - result[i - 1]) / (2 * h);
                var y = result[i];
                max = Math.Max(max,
                    Math.Abs(y2 * p(res) + y1 * q(res) + y * r(res) - f(res)));
            }
            Console.WriteLine(max);
            Console.ReadKey();
            //Console.WriteLine (result);
        }
    }
}
