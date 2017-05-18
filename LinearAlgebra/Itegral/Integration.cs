using System;
using System.Collections.Generic;
using LinearAlgebra.Helpers;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace Integral
{
    public static class Integration
    {
        private static double ApplyComposite(double a, double b, int m,
            Func<double, double> func,
            Func<double, double, Func<double, double>, double> method)
        {
            double z = a;
            double h = (b - a) / m;

            double result = 0;

            for (int i = 0; i < m; i++)
            {
                result += method(z, z += h, func);
            }
            return result;
        }

        public static double LeftRectangle(double a, double b, Func<double, double> func)
        {
            return (b - a) * func(a);
        }

        public static double CompositeLeftRectangle(double a, double b, int m, Func<double, double> func)
        {
            return ApplyComposite(a, b, m, func, LeftRectangle);
        }

        public static double MiddleRectangle(double a, double b, Func<double, double> func)
        {
            return (b - a) * func((a + b) / 2);
        }

        public static double CompositeMiddleRectangle(double a, double b, int m, Func<double, double> func)
        {
            return ApplyComposite(a, b, m, func, MiddleRectangle);
        }

        public static double RightRectangle(double a, double b, Func<double, double> func)
        {
            return (b - a) * func(b);
        }

        public static double CompositeRightRectangle(double a, double b, int m, Func<double, double> func)
        {
            return ApplyComposite(a, b, m, func, RightRectangle);
        }

        public static double Trapeze(double a, double b, Func<double, double> func)
        {
            return 0.5 * (b - a) * (func(a) + func(b));
        }

        public static double CompositeTrapeze(double a, double b, int m, Func<double, double> func)
        {
            return ApplyComposite(a, b, m, func, Trapeze);
        }

        public static double Simpson(double a, double b, Func<double, double> func)
        {
            return (b - a) * (func(a) + 4 * func((a + b) / 2) + func(b)) / 6;
        }

        public static double CompositeSimpson(double a, double b, int m, Func<double, double> func)
        {
            return ApplyComposite(a, b, m, func, Simpson);
        }

        public static GaussInfo Gauss2(double a, double b,
            Func<double, double> w, Func<double, double> f,
            Func<Func<double, double>, int, double, double, double> integral)
        {
            return Gauss(a, b, w, f, integral,
                coefficients =>
                    LinearAlgebra.Helpers.MathHelper.SolveQuadraticEquation(
                        coefficients[0],
                        coefficients[1]),
                2);
        }

        public static GaussInfo Gauss3(double a, double b,
            Func<double, double> w, Func<double, double> f,
            Func<Func<double, double>, int, double, double, double> integral)
        {
            return Gauss(a, b, w, f, integral,
                coefficients =>
                    MathHelper.SolveCubicEquation(
                        coefficients[0],
                        coefficients[1],
                        coefficients[2]),
                3);
        }

        public static GaussInfo GaussN(double a, double b,
            Func<double, double> w, Func<double, double> f,
            Func<Func<double, double>, int, double, double, double> integral, int n)
        {
            switch (n)
            {
                case 2:
                    return Gauss2(a, b, w, f, integral);
                case 3:
                    return Gauss3(a, b, w, f, integral);
                default:
                    return Gauss(a, b, w, f, integral,
                        coefficients => MathHelper.Bisection(
                            a, b, coefficients.CalculateUnaryPolynomialValue, Math.Pow(10, -10)),
                        n);
            }
        }

        public static double CalculateUnaryPolynomialValue(this double[] coefficients, double x)
        {
            int n = coefficients.Length;
            double value = Math.Pow(x,n);

            for (int i = 0; i < n; i++)
            {
                value += coefficients[i] * Math.Pow(x, n - i - 1);
            }
            return value;
        }

        public static GaussInfo Gauss(double a, double b,
            Func<double, double> w, Func<double, double> f,
            Func<Func<double, double>, int, double, double, double> integral,
            Func<double[], double[]> solveEquation, int n)
        {
            double[] moments = GetMomentsForGauss(a, b, w, n, integral);

            double[] polynomialCoefficients = GetPolynomialCoefficientsForGauss(moments, n);
            double[] nodes = solveEquation(polynomialCoefficients);

            double[] quadratureFormCoefficients = GetQuadratureFormCoefficientsForGauss(moments, n, nodes);

            double result = CalculateReuslt(quadratureFormCoefficients, f, nodes, n);

            return new GaussInfo(moments, polynomialCoefficients, nodes, quadratureFormCoefficients, result);
        }

        public static GaussInfo Gauss(double a, double b, Func<double, double> f, int n)
        {
            return Gauss(a, b, f, polynomial => MathHelper.Bisection(
                -1, 1, polynomial, Math.Pow(10, -10)), n);
        }

        public static GaussInfo Gauss(double a, double b, Func<double, double> f,
            Func<Func<double, double>, double[]> solveEquation, int n)
        {
            double[] nodes = solveEquation(x => MathHelper.LegendrePolynomial(x, n));
            double[] quadratureFormCoefficients = GetQuadratureFormCoefficientsForGauss(nodes);

            ConvertNodesAndQuadratureCoefficients(nodes, quadratureFormCoefficients, a, b);
            double result = CalculateReuslt(quadratureFormCoefficients, f, nodes, n);

            return new GaussInfo(null, null, nodes, quadratureFormCoefficients, result);
        }

        private static void ConvertNodesAndQuadratureCoefficients(double[] nodes,
            double[] quadratureFormCoefficients, double a, double b)
        {
            for (int i = 0; i < nodes.Length; i++)
            {
                nodes[i] = (b - a) * nodes[i] / 2 + (b + a) / 2;
                quadratureFormCoefficients[i] = (b - a) * quadratureFormCoefficients[i] / 2;
            }
        }

        private static double CalculateReuslt(double[] quadratureFormCoefficients,
            Func<double, double> f, double[] nodes, int n)
        {
            double result = 0;
            for (int i = 0; i < n; i++)
                result += quadratureFormCoefficients[i] * f(nodes[i]);

            return result;
        }

        private static double[] GetMomentsForGauss(double a, double b,
            Func<double, double> w, int n,
            Func<Func<double, double>, int, double, double, double> integral)
        {
            int count = 2 * n;
            double[] moments = new double[count];
            for (int i = 0; i < count; i++)
                moments[i] = integral(w, i, a, b);

            return moments;
        }

        private static double[] GetPolynomialCoefficientsForGauss(double[] moments, int n)
        {
            double[,] coefficients = new double[n, n];
            double[] values = new double[n];
            for (int i = 0; i < n; i++)
            {
                values[i] = -moments[n + i];
                for (int j = 0; j < n; j++)
                    coefficients[i, j] = moments[n - j + i - 1];
            }
            return SystemLinearEquation.Solver.GaussSolveSystem(Matrix.Create(coefficients),
                Vector.Create(values)).ToArray();
        }

        private static double[] GetQuadratureFormCoefficientsForGauss(double[] moments, int n, double[] nodes)
        {
            double[,] coefficients = new double[n, n];
            double[] values = new double[n];
            for (int i = 0; i < n; i++)
            {
                values[i] = moments[i];
                for (int j = 0; j < n; j++)
                    coefficients[i, j] = Math.Pow(nodes[j], i);
            }

            return SystemLinearEquation.Solver.GaussSolveSystem(Matrix.Create(coefficients), 
                Vector.Create(values)).ToArray();
        }

        private static double[] GetQuadratureFormCoefficientsForGauss(double[] nodes)
        {
            double[] quadratureCoefficients = new double[nodes.Length];

            for (int i = 0; i < nodes.Length; i++)
            {
                quadratureCoefficients[i] = 2 / ((1 - Math.Pow(nodes[i],2)) *
                                                 Math.Pow(MathHelper.DerivativeLegendrePolynomial(nodes[i], nodes.Length), 2));
            }
            return quadratureCoefficients;
        }
    }


}