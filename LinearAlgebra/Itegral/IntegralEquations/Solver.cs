using System;
using Integral;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace Integral.IntegralEquations
{
    public static class IntegralEquationSolver
    {
        public static Func<double, double> MechanicalQuadratureMethod(
            Func<double, double, double> k,
            Func<double, double> f,
            double a, double b, int n)
        {
            Matrix matrix = Matrix.CreateIdentity(n);
            Vector vector = Vector.CreateZero(n);

            var tmpGauss = Integration.Gauss(a, b, value => 1, n);
            double[] coef = tmpGauss.QuadratureFormCoefficients;
            double[] nodes = tmpGauss.Nodes;

            for (int i = 1; i <= n; i++)
            {
                double xi = nodes[i - 1];
                vector[i] = f(xi);
                for (int j = 1; j <= n; j++)
                {
                    double xj = nodes[j - 1];
                    matrix[i, j] = coef[j-1] * k(xi, xj);
                }
                matrix[i, i] += 1;
            }
            Console.WriteLine(matrix);
            Console.WriteLine(vector);
            Vector res = SystemLinearEquation.Solver.GaussSolveSystem(matrix, vector);

            Console.WriteLine(res);
            return value =>
            {
                double sum = 0;

                for (int i = 1; i <= n; i++)
                {
                    sum += -coef[i-1] * k(value, (nodes[i-1])) * res[i];
                }
                return f(value) + sum;
            };
        }

        public static Func<double, double> KernelReplacementMethod(
            Func<double, double>[] alpha,
            Func<double, double>[] beta,
            Func<double, double> f,
            double a, double b, int q)
        {
            if (alpha.Length < q || beta.Length < q)
            {
                throw new AggregateException();
            }
            alpha = alpha.Clone() as Func<double, double>[];
            beta = beta.Clone() as Func<double, double>[];

            int m = 20;
            Matrix matrix = Matrix.CreateIdentity(q);
            Vector vector = Vector.CreateZero(q);

            for (int i = 1; i <= q; i++)
            {
                var i1 = i;
                vector[i] = Integration.CompositeSimpson(a, b, m, x => beta[i1-1](x) * f(x));
                for (int j = 1; j <= q; j++)
                {
                    var j1 = j;
                    matrix[i, j] = Integration.CompositeSimpson(a, b, m, x => beta[i1 -1](x) * alpha[j1 - 1](x));
                }
                matrix[i, i] += 1;
            }
            Vector res = SystemLinearEquation.Solver.GaussSolveSystem(matrix, vector);

            return value =>
            {
                double sum = 0;

                for (int i = 1; i <= q; i++)
                {
                    sum += alpha[i-1](value) * res[i];
                }
                return f(value) - sum;
            };
        }
    }
}