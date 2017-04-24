using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Helpers;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace SystemLinearEquation
{
    public static class Solver
    {
        public static readonly double Epsilon = Math.Pow(10, -8);

        public static Vector TridiagonalMethod(Matrix matrix, Vector vector)
        {
            if (matrix.Dimension != vector.Dimension && matrix.Dimension < 2)
            {
                throw new ArgumentException();
            }
            int n = vector.Dimension;

            Vector m = Vector.CreateZero(n);
            Vector k = Vector.CreateZero(n);

            m[1] = -matrix[1, 2] / matrix[1, 1];
            k[1] = vector[1] / matrix[1, 1];

            for (int i = 2; i <= n; i++)
            {
                double a = i != 0 ? matrix[i, i - 1] : 0;
                double b = matrix[i, i];
                double c = i != n ? matrix[i, i + 1] : 0;
                double d = vector[i];

                double temp = a * m[i - 1] + b;
                m[i] = -c / temp;
                k[i] = (d - k[i - 1] * a) / temp;
            }

            Vector y = Vector.CreateZero(n);
            y[n] = k[n];

            for (int i = n - 1; i >= 1; i--)
            {
                y[i] = m[i] * y[i + 1] + k[i];
            }
            return y;
        }

        public static double Determinant(Matrix oldMatrix)
        {
            Matrix matrix = oldMatrix.Clone();
            for (int k = 1; k <= matrix.Dimension; k++)
            {
                double coeff = matrix[k, k];

                for (int j = k; j <= matrix.Dimension; j++)
                {
                    matrix[k, j] = matrix[k, j] / coeff;
                }

                double tmp = 0;
                for (int i = k + 1; i <= matrix.Dimension; i++)
                {
                    tmp = matrix[i, k];
                    for (int j = k; j <= matrix.Dimension; j++)
                    {
                        matrix[i, j] = matrix[i, j] - matrix[k, j] * tmp;
                    }
                }

                matrix[k, k] = coeff;
            }

            double result = 1;
            for (int k = 1; k <= matrix.Dimension; k++)
            {
                result *= matrix[k, k];
            }
            return result;
        }

        public static Vector GaussSolveSystem(Matrix oldMatrix, Vector oldVector)
        {
            Matrix matrix = oldMatrix.Clone();
            Vector vector = oldVector.Clone();
            if (matrix.Dimension != vector.Dimension)
            {
                throw new ArgumentException();
            }

            for (int k = 1; k <= matrix.Dimension; k++)
            {
                if (Math.Abs(matrix[k, k]) <= Epsilon)
                {
                    Console.WriteLine($"Warning! a({k}, {k}) <= {Epsilon}");
                }
                double tmp = matrix[k, k];

                for (int j = k; j <= matrix.Dimension; j++)
                {
                    matrix[k, j] = matrix[k, j] / tmp;
                }
                vector[k] = vector[k] / tmp;
                
                for (int i = k + 1; i <= matrix.Dimension; i++)
                {
                    tmp = matrix[i, k];
                    for (int j = k; j <= matrix.Dimension; j++)
                    {
                        matrix[i, j] = matrix[i, j] - matrix[k, j]*tmp;
                    }
                    vector[i] = vector[i] - vector[k]*tmp;
                }
            }
            Vector result = Vector.CreateZero(vector.Dimension);

            for (int i = matrix.Dimension; i > 0; i--)
            {
                double tmp = 0;
                for (int j = i + 1; j <= matrix.Dimension; j++)
                {
                    tmp += matrix[i, j] * result[j];
                }
                result[i] = vector[i] - tmp;
            }

            return result;
        }

        public static Vector GaussWithChoiceSolveSystem(Matrix oldMatrix, Vector oldVector)
        {
            Matrix matrix = oldMatrix.Clone();
            Vector vector = oldVector.Clone();
            Vector addr = Vector.CreateZero(oldVector.Dimension);
            for (int i = 1; i <= oldVector.Dimension; i++)
            {
                addr[i] = i;
            }
            
            for (int k = 1; k <= matrix.Dimension; k++)
            {
                int index = k;
                for (int i = k; i <= matrix.Dimension; i++)
                {
                    if (Math.Abs(matrix[k, (int) addr[i]]) > Math.Abs(matrix[k, (int) addr[index]]))
                    {
                        index = i;
                    }
                }

                int tmpIndex = (int)addr[k];
                addr[k] = addr[index];
                addr[index] = tmpIndex;

                if (Math.Abs(matrix[k, (int)addr[k]]) <= Epsilon)
                {
                    Console.WriteLine($"Warning! a({k}, {k}) <= {Epsilon}");
                }

                double tmp = matrix[k, (int)addr[k]];

                for (int j = k; j <= matrix.Dimension; j++)
                {
                    matrix[k, (int)addr[j]] = matrix[k, (int)addr[j]] / tmp;
                }
                vector[k] = vector[k] / tmp;

                for (int i = k + 1; i <= matrix.Dimension; i++)
                {
                    tmp = matrix[i, (int)addr[k]];
                    for (int j = k; j <= matrix.Dimension; j++)
                    {
                        matrix[i, (int)addr[j]] = matrix[i, (int)addr[j]] - matrix[k, (int)addr[j]] * tmp;
                    }
                    vector[i] = vector[i] - vector[k] * tmp;
                }
            }
            Vector result = Vector.CreateZero(vector.Dimension);

            for (int i = matrix.Dimension; i > 0; i--)
            {
                double tmp = 0;
                for (int j = i + 1; j <= matrix.Dimension; j++)
                {
                    tmp += matrix[i, (int)addr[j]] * result[(int)addr[j]];
                }
                result[(int)addr[i]] = vector[i] - tmp;
            }

            return result;
        }

        public static Matrix Inverse(Matrix oldMatrix)
        {
            Matrix matrix = oldMatrix.Clone();
            Matrix identity = Matrix.CreateIdentity(oldMatrix.Dimension);
            Vector addr = Vector.CreateZero(matrix.Dimension);
            for (int i = 1; i <= addr.Dimension; i++)
            {
                addr[i] = i;
            }

            for (int k = 1; k <= matrix.Dimension; k++)
            {
                int index = k;
                for (int i = k; i <= matrix.Dimension; i++)
                {
                    if (Math.Abs(matrix[k, (int)addr[i]]) > Math.Abs(matrix[k, (int)addr[index]]))
                    {
                        index = i;
                    }
                }

                int tmpIndex = (int)addr[k];
                addr[k] = addr[index];
                addr[index] = tmpIndex;

                if (Math.Abs(matrix[k, (int)addr[k]]) <= Epsilon)
                {
                    Console.WriteLine($"Warning! a({k}, {k}) <= {Epsilon}");
                }

                double tmp = matrix[k, (int)addr[k]];

                for (int j = k; j <= matrix.Dimension; j++)
                {
                    matrix[k, (int)addr[j]] = matrix[k, (int)addr[j]] / tmp;
                }
                for (int j = 1; j <= identity.Dimension; j++)
                {
                    identity[k, j] = identity[k, j] / tmp;
                }

                for (int i = k + 1; i <= matrix.Dimension; i++)
                {
                    tmp = matrix[i, (int)addr[k]];
                    for (int j = k; j <= matrix.Dimension; j++)
                    {
                        matrix[i, (int)addr[j]] = matrix[i, (int)addr[j]] - matrix[k, (int)addr[j]] * tmp;
                    }

                    for (int j = 1; j <= identity.Dimension; j++)
                    {
                        identity[i, j] = identity[i, j] - identity[k, j] * tmp;
                    }
                }
            }
            Matrix inverse = Matrix.CreateIdentity(matrix.Dimension);
            
            for (int i = matrix.Dimension; i > 0; i--)
            {
                for (int q = 1; q <= inverse.Dimension; q++)
                {
                    double tmp = 0;
                    for (int j = i + 1; j <= matrix.Dimension; j++)
                    {
                        tmp += matrix[i, (int)addr[j]] * inverse[(int)addr[j], q];
                    }
                    inverse[(int)addr[i], q] = identity[i, q] - tmp;
                }
            }
            return inverse;
        }

        public static Vector SimpleIteration(Matrix oldMatrix, Vector oldVector)
        {
            oldMatrix.NotNull();
            oldVector.NotNull();

            Matrix matrix = oldMatrix.Clone();
            Vector vector = oldVector.Clone();

            Matrix h = GetH(matrix);
            Vector g = GetG(matrix, vector);

            Console.WriteLine($"H = \n{h}\n");
            double normH = h.Norm(Matrix.NumP.Infinity);
            Console.WriteLine($"||H|| = {normH}\n");
            Console.WriteLine($"g = \n{g}");

            Vector x = Vector.CreateZero(oldVector.Dimension);
            Vector temp;

            int count = 0;
            do
            {
                count++;
                temp = x.Clone();

                x = Vector.Sum(Matrix.MultiplyOnVector(h,temp), g);
            } while (normH / (1-normH)*Vector.Substraction(x, temp).Norm(Vector.NumP.Infinity) >= Epsilon);

            Console.WriteLine($"Количество приближений: {count}\n");
            return x;
        }

        public static Vector Seidel(Matrix oldMatrix, Vector oldVector)
        {
            oldMatrix.NotNull();
            oldVector.NotNull();

            Matrix matrix = oldMatrix.Clone();
            Vector vector = oldVector.Clone();

            Matrix h = GetH(matrix);
            Vector g = GetG(matrix, vector);

            Console.WriteLine($"H = \n{h}\n");
            double normH = h.Norm(Matrix.NumP.Infinity);
            Console.WriteLine($"||H|| = {normH}\n");
            Console.WriteLine($"g = \n{g}");

            Vector x = Vector.CreateZero(vector.Dimension);
            Vector temp;

            int count = 0;
            do
            {
                count++;
                temp = x.Clone();
                for (int i = 1; i <= x.Dimension; i++)
                {
                    double sum = 0;
                    for (int j = 1; j < i; j++)
                    {
                        sum += h[i, j] * x[j];
                    }
                    for (int j = i; j <= x.Dimension; j++)
                    {
                        sum += h[i, j] * x[j];
                    }
                    x[i] = sum + g[i];
                }
            } while (normH / (1 - normH) * Vector.Substraction(x, temp).Norm(Vector.NumP.Infinity) >= Epsilon);

            Console.WriteLine($"Количество приближений: {count}\n");
            return x;
        }

        private static Vector GetG(Matrix a, Vector b)
        {
            double[] g = new double[b.Dimension];

            for (int i = 0; i < b.Dimension; i++)
            {
                g[i] = b[i+1] / a[i+1, i+1];
            }
            return Vector.Create(g);
        }

        private static Matrix GetHr(Matrix h)
        {
            double[,] hr = new double[h.Dimension, h.Dimension];

            for (int i = 1; i < hr.GetHeight(); i++)
            {
                for (int j = 0; j < i; j++)
                {
                    hr[j, i] = h[j, i];
                }
            }
            return Matrix.Create(hr);
        }

        private static Matrix GetHl(Matrix h)
        {
            double[,] hl = new double[h.Dimension, h.Dimension];

            for (int i = 0; i < hl.GetHeight(); i++)
            {
                for (int j = 0; j < i; j++)
                {
                    hl[i, j] = h[i, j];
                }
            }
            return Matrix.Create(hl);
        }

        private static Matrix GetH(Matrix a)
        {
            double[,] h = new double[a.Dimension, a.Dimension];

            for (int i = 0; i < h.GetHeight(); i++)
            {
                for (int j = 0; j < h.GetWidth(); j++)
                {
                    h[i, j] = i != j ? -a[i+1, j+1] / a[i+1, i+1] : 0;
                }
            }
            return Matrix.Create(h);
        }
    }
}
