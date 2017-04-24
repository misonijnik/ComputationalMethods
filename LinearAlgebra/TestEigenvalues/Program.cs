using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using SystemLinearEquation;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace TestEigenvalues
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Решение полной и частичной проблемы собственных значений.");
            Console.WriteLine("Матрица:");
            Matrix matrix = Matrix.Create(new double[,]
            {
                {-0.951210, -0.097787, 0.358427},
                {-0.097787,  0.615454, 0.022286},
                {0.358427,  0.022286, -0.957290}
            });
            Console.WriteLine(matrix);
            Console.WriteLine("Нахождение максимального по модуля собственного числа:");
            Console.WriteLine();
            Console.WriteLine("Степенным методом:");
            Vector resultVec;
            double result = Eigenvalues.EigenvalueSeeker.Iteration(matrix, out resultVec);
            Console.WriteLine("Собственное число:");
            Console.WriteLine(result);
            Console.WriteLine("Собственный вектор:");
            Console.WriteLine(resultVec);
            Console.WriteLine("Вектор невязки:");
            Vector nev = Vector.Substraction(
                Matrix.MultiplyOnVector(matrix, resultVec),
                Vector.MultiplyOnScalar(result, resultVec));
            Console.WriteLine(nev);
            Console.WriteLine();
            Console.WriteLine("Методом скалярных произведений:");
            result = Eigenvalues.EigenvalueSeeker.ScalarMultiply(matrix, out resultVec);
            Console.WriteLine("Собственное число:");
            Console.WriteLine(result);
            Console.WriteLine("Собственный вектор:");
            Console.WriteLine(resultVec);
            Console.WriteLine("Вектор невязки:");
            nev = Vector.Substraction(
                Matrix.MultiplyOnVector(matrix, resultVec),
                Vector.MultiplyOnScalar(result, resultVec));
            Console.WriteLine(nev);

            result = Eigenvalues.EigenvalueSeeker.Iteration(matrix, out resultVec);
            double opposite = Eigenvalues.EigenvalueSeeker.OppositeSpectrumBoundary(matrix, result);
            Console.WriteLine("Противоположная граница спектра:");
            Console.WriteLine(opposite);

            if (!opposite.Equals(result))
            {
                double third = Solver.Determinant(matrix) / (opposite * result);
                Console.WriteLine("Недостающее собственное число:");
                Console.WriteLine(third);
            }
            Console.WriteLine();
            Console.WriteLine("Метод вращений:");
            Matrix resultMatrix;
            double[] hmm = Eigenvalues.EigenvalueSeeker.Rotation(matrix, out resultMatrix);
            Console.WriteLine("Собственные числа:");
            foreach (var d in hmm)
            {
                Console.WriteLine(d);
            }
            Console.WriteLine("Матрица собственных векторов:");
            Console.WriteLine(resultMatrix);
            for (int i = 1; i <= resultMatrix.Dimension; i++)
            {
                resultVec[i] = resultMatrix[i,1];
            }
            Console.WriteLine("Вектор невязки:");
            nev = Vector.Substraction(
                Matrix.MultiplyOnVector(matrix, resultVec),
                Vector.MultiplyOnScalar(hmm[0], resultVec));
            Console.WriteLine(nev);
            Console.ReadKey();
        }
    }
}
