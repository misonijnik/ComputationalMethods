using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;
using SystemLinearEquation;

namespace DifferentialEquationSolver
{
    public static class DifferentialEquation
    {
        public static Vector DifferenceMethod1(
            Func<double, double> p,
            Func<double, double> q,
            Func<double, double> r,
            Func<double, double> f,
            double a, double b, int n,
            double alpha0, double alpha1, double A,
            double betta0, double betta1, double B)
        {
            double h = (b - a) / n;

            Matrix matrix = Matrix.CreateIdentity(n + 1);
            Vector vector = Vector.CreateZero(n + 1);

            matrix[1, 1] = h * alpha0 - alpha1;
            matrix[1, 2] = alpha1;
            vector[1] = h * A;

            for (int i = 2; i <= n; i++)
            {
                double x = a + h * i;
                matrix[i, i - 1] = p(x) - h * q(x) / 2;
                matrix[i, i] = -2 * p(x) + h * h * r(x);
                matrix[i, i + 1] = p(x) + h * q(x) / 2;
                vector[i] = h * h * f(x);
            }

            matrix[n+1, n] = -betta1;
            matrix[n+1, n+1] = h * betta0 + betta1;
            vector[n+1] = h * B;

            return Solver.TridiagonalMethod(matrix, vector);
        }

        public static Vector DifferenceMethod2(
            Func<double, double> p,
            Func<double, double> q,
            Func<double, double> r,
            Func<double, double> f,
            double a, double b, int n,
            double alpha0, double alpha1, double A,
            double betta0, double betta1, double B)
        {
            double h = (b - a) / n;

            Matrix matrix = Matrix.CreateIdentity(n + 1);
            Vector vector = Vector.CreateZero(n + 1);

            for (int i = 2; i <= n; i++)
            {
                double x = a + h * i;
                matrix[i, i - 1] = p(x) - h * q(x) / 2;
                matrix[i, i] = -2 * p(x) + h * h * r(x);
                matrix[i, i + 1] = p(x) + h * q(x) / 2;
                vector[i] = h * h * f(x);
            }

            matrix[1, 1] = 2 * h * alpha0 + alpha1 * (matrix[2, 1] / matrix[2, 3] - 3);
            matrix[1, 2] = alpha1 * (matrix[2, 2] / matrix[2, 3] + 4);
            vector[1] = 2 * h * A + alpha1 * (vector[2] / matrix[2, 3]);

            matrix[n+1, n] = -betta1 * (4 + matrix[n, n] / matrix[n, n - 1]);
            matrix[n+1, n+1] = 2 * h * betta0 + betta1 * (3 - matrix[n, n+1] / matrix[n, n - 1]);
            vector[n+1] = 2 * h * B - betta1 * (vector[n] / matrix[n, n -1]);

            return Solver.TridiagonalMethod(matrix, vector);
        }
    }
}
