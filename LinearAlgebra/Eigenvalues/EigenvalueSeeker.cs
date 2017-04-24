using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Helpers;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace Eigenvalues
{
    public static class EigenvalueSeeker
    {
        public static readonly double Epsilon1 = Math.Pow(10, -7);
        public static readonly double Epsilon2 = Math.Pow(10, -5);

        private static double MaxModuleMatrix(Matrix matrix, out int ik, out int jk)
        {
            ik = 1;
            jk = 2;

            double max = matrix[ik, jk];
            for (int i = 1; i <= matrix.Dimension; i++)
            {
                for (int j = i+1; j <= matrix.Dimension; j++)
                {
                    if (max < Math.Abs(matrix[i, j]))
                    {
                        max = Math.Abs(matrix[i, j]);
                        ik = i;
                        jk = j;
                    }
                }
            }
            return matrix[ik, jk];
        }

        public static double[] Rotation(Matrix oldMatrix, out Matrix resultMatrix)
        {
            Matrix matrix = oldMatrix.Clone();
            Matrix result = Matrix.CreateIdentity(oldMatrix.Dimension);
            Func<Matrix, int, int, double> d = (m, i, j) => Math.Sqrt(
                Math.Pow(m[i, i] - m[j, j], 2) + 4 * m[i, j] * m[i, j]);
            Func<Matrix, int, int, double> c = (m, i, j) => Math.Sqrt(
                0.5 * (1 + Math.Abs(m[i, i] - m[j, j]) / d(m, i, j)));
            Func<Matrix, int, int, double> s = (m, i, j) => Math.Sign(
                m[i, j] * (m[i, i] - m[j, j])) *
                Math.Sqrt(
                    0.5 * (1 - Math.Abs(m[i, i] - m[j, j]) / d(m, i, j)));
            Func<Matrix, int, int, Matrix> v = (m, i, j) =>
            {
                Matrix r = Matrix.CreateIdentity(m.Dimension);
                r[i, i] = r[j, j] = c(m, i, j);
                r[i, j] = -s(m, i, j);
                r[j, i] = -r[i, j];
                return r;
            };

            int k = 0;
            int ik;
            int jk;
            double ck;
            double sk;

            while (true)
            {
                k++;
                double max = MaxModuleMatrix(matrix, out ik, out jk);
                if (Math.Abs(max) < Epsilon2)
                {
                    break;
                }
                ck = c(matrix, ik, jk);
                sk = s(matrix, ik, jk);
                Matrix newMatrix = matrix.Clone();
                for (int i = 1; i <= matrix.Dimension; i++)
                {
                    if (i != ik && i != jk)
                    {
                        newMatrix[i, ik] = newMatrix[ik, i] = ck * matrix[i, ik] + sk * matrix[i, jk];
                        newMatrix[i, jk] = newMatrix[jk, i] = -sk * matrix[i, ik] + ck * matrix[i, jk];
                    }
                }
                newMatrix[ik, ik] = ck * ck * matrix[ik, ik] + 2 * ck * sk * matrix[ik, jk] + sk * sk * matrix[jk, jk];
                newMatrix[jk, jk] = sk * sk * matrix[ik, ik] - 2 * ck * sk * matrix[ik, jk] + ck * ck * matrix[jk, jk];
                newMatrix[ik, jk] = newMatrix[jk, ik] = (ck * ck - sk * sk) * matrix[ik, jk] + ck * sk * (matrix[jk, jk] - matrix[ik, ik]);
                result = Matrix.Multiply(result, v(matrix, ik, jk));
                matrix = newMatrix;
            }
            Console.WriteLine("Количество итераций:");
            Console.WriteLine(k);
            Console.WriteLine(matrix);
            List<double> res = new List<double>();
            for (int i = 1; i <= matrix.Dimension; i++)
            {
                res.Add(matrix[i,i]);
            }
            resultMatrix = result;
            return res.ToArray();
        }

        public static double Iteration(Matrix oldMatrix, out Vector resultVector)
        {
            Vector y = Vector.CreateZero(oldMatrix.Dimension);
            double yd = 0;
            double nextYd = 0;
            for (int i = 1; i <= y.Dimension; i++)
            {
                y[i] = 1;
            }
            yd = y.Norm(Vector.NumP.Infinity);
            Vector nextY = Matrix.MultiplyOnVector(oldMatrix, y);
            nextYd = nextY.MaxModule();
            nextY = Vector.MultiplyOnScalar(1 / nextYd, nextY);

            int count = 0;
            while (Math.Abs(Math.Abs(yd) - Math.Abs(nextYd)) > Epsilon1)
            {
                count++;
                y = nextY;
                yd = nextYd;
                nextY = Matrix.MultiplyOnVector(oldMatrix, y);
                nextYd = nextY.MaxModule();
                nextY = Vector.MultiplyOnScalar(1 / nextYd, nextY);
            }
            Console.WriteLine("Количество итераций:");
            Console.WriteLine(count);
            resultVector = y;
            return nextYd;
        }

        public static double ScalarMultiply(Matrix oldMatrix, out Vector resultVector)
        {
            Vector y = Vector.CreateZero(oldMatrix.Dimension);
            double yd = 0;
            double nextYd = 0;
            for (int i = 1; i <= y.Dimension; i++)
            {
                y[i] = 1;
            }
            yd = y.Norm(Vector.NumP.Infinity);
            Vector nextY = Matrix.MultiplyOnVector(oldMatrix, y);
            nextYd = Vector.ScalarProduct(nextY, y) / Vector.ScalarProduct(y, y);
            int count = 0;
            while (Math.Abs(Math.Abs(yd) - Math.Abs(nextYd)) > Epsilon1)
            {
                count++;
                y = Vector.MultiplyOnScalar(1 / nextY.MaxModule(), nextY);
                yd = nextYd;
                nextY = Matrix.MultiplyOnVector(oldMatrix, y);
                nextYd = Vector.ScalarProduct(nextY, y) / Vector.ScalarProduct(y, y);
            }
            Console.WriteLine("Количество итераций:");
            Console.WriteLine(count);
            resultVector = Vector.MultiplyOnScalar(1 / nextY.MaxModule(), nextY);
            return nextYd;
        }

        public static double OppositeSpectrumBoundary(Matrix oldMatrix, double lambda)
        {
            Matrix matrix = Matrix.Substraction(oldMatrix, Matrix.MultiplyOnScalar(
                lambda,
                Matrix.CreateIdentity(oldMatrix.Dimension)));
            Vector vec = Vector.CreateZero(oldMatrix.Dimension);
            return lambda + Iteration(matrix, out vec);
        }


    }
}
