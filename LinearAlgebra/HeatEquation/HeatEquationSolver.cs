using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SystemLinearEquation;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace HeatEquation
{
    public static class HeatEquationSolver
    {
        public static double[,] HeatEquation1(
            double tMax, double a, int n, int m,
            Func<double, double, double> f,
            Func<double, double> g,
            Func<double, double> alpha,
            Func<double, double> betta)
        {
            double h = 1.0d / n;
            double tau = tMax / m;

            if (tau > h*h / (2 * a))
            {
                Console.WriteLine("Не выполнено условие устойчивости");
            }

            double[,] u = new double[n + 1, m + 1];
            for (int i = 0; i <= n; i++)
            {
                double xi = i * h;
                u[i, 0] = g(xi);
            }
            for (int k = 0; k <= m; k++)
            {
                double tk = k * tau;
                u[0, k] = alpha(tk);
                u[n, k] = betta(tk);
            }
            for (int k = 1; k <= m; k++)
            {
                double tk = k * tau;
                for (int i = 1; i < n; i++)
                {
                    double xi = i * h;
                    u[i, k] = (1 - 2 * a * tau / (h*h)) * u[i, k - 1] +
                              (a * tau / (h * h)) * (u[i - 1, k - 1] + u[i + 1, k - 1]) +
                              tau * f(xi, tk - tau);
                }
            }
            return u;
        }

        public static double[,] HeatEquation2(
            double tMax, double a, int n, int m,
            Func<double, double, double> f,
            Func<double, double> g,
            Func<double, double> alpha,
            Func<double, double> betta)
        {
            double h = 1.0d / n;
            double tau = tMax / m;

            if (tau > (h * h) / (2 * a))
            {
                Console.WriteLine("Не выполнено условие устойчивости");
            }

            double[,] u = new double[n + 1, m + 1];
            for (int i = 0; i <= n; i++)
            {
                double xi = i * h;
                u[i, 0] = g(xi);
            }
            for (int k = 1; k <= m; k++)
            {
                Matrix matrix = Matrix.CreateIdentity(n + 1);
                Vector vector = Vector.CreateZero(n + 1);

                double tk = tau * k;

                vector[1] = alpha(tk);
                vector[n+1] = betta(tk);

                for (int i = 2; i <= n; i++)
                {
                    double xi = (i-1) * h;
                    double temp = -a * tau / (h*h);
                    matrix[i, i - 1] = matrix[i, i + 1] = temp;
                    matrix[i, i] -= 2 * temp;
                    vector[i] = tau * f(xi, tk) + u[i - 1, k - 1];
                }
                var res = Solver.TridiagonalMethod(matrix, vector);
                for (int i = 0; i <= n; i++)
                {
                    u[i, k] = res[i+1];
                }
            }
            return u;
        }
    }
}
