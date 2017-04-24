using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace Obuslov
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Рассмотрим исходную систему Ax = b и систему с возмущенной правой частью Ax' = (b + deltBb)");
            Console.WriteLine("A =");
            Matrix a = Matrix.Create(new [,]
            {
                {1.00, 0.99},
                {0.99, 0.98}
            });
            Console.WriteLine(a);
            Console.WriteLine();
            Console.WriteLine("b =");
            Vector b = Vector.Create(new double[] { 1.99, 1.97 });
            Console.WriteLine(b);
            Console.WriteLine();
            Console.WriteLine("deltaB =");
            Vector deltaB = Vector.Create(new double[] { -0.000097, 0.000106 });
            Console.WriteLine(deltaB);
            Console.WriteLine();
            Console.WriteLine("x =");
            Vector x = Vector.Create(new double[] { 1, 1 });
            Console.WriteLine(x);
            Console.WriteLine("x' =");
            Vector x1 = Vector.Create(new[] { 3.0000, -1.0203 });
            Console.WriteLine(x1);
            Console.WriteLine();
            Console.WriteLine("Векторы невязки:");
            Console.WriteLine("r = Ax - b =");
            Vector r = Vector.Substraction(Matrix.MultiplyOnVector(a, x), b);
            Console.WriteLine(r);
            Console.WriteLine();
            Console.WriteLine("r' = Ax' - (b + deltaB)");
            Vector r1 = Vector.Substraction(Matrix.MultiplyOnVector(a, x1), Vector.Sum(b, deltaB));
            Console.WriteLine(r1);
            Console.WriteLine();
            Console.WriteLine("Фактическая относительная погрешность:");
            Console.WriteLine("sigmaX = |x' - x|/|x|");
            Console.WriteLine();
            Console.WriteLine("(p = 1)");
            double sigmaX = Vector.Substraction(x1, x).Norm(Vector.NumP.One) / x.Norm(Vector.NumP.One);
            Console.WriteLine(sigmaX);
            Console.WriteLine("(p = infinity)");
            double sigmaX1 = Vector.Substraction(x1, x).Norm(Vector.NumP.Infinity) / x.Norm(Vector.NumP.Infinity);
            Console.WriteLine(sigmaX1);
            Console.WriteLine();
            double detA = -0.0001;
            Matrix reverseA = Matrix.Create(new[,]
            {
                {0.98/detA,  -0.99/detA},
                {-0.99/detA, 1.00/detA}
            });
            Console.WriteLine("Число обсловленности матрицы A:");
            Console.WriteLine("q(A) = |A|*|A^(-1)|");
            Console.WriteLine();
            Console.WriteLine("(p = 1)");
            double q = a.Norm(Matrix.NumP.One) * reverseA.Norm(Matrix.NumP.One);
            Console.WriteLine(q);
            Console.WriteLine();
            Console.WriteLine("(p = infinity)");
            double q1 = a.Norm(Matrix.NumP.Infinity) * reverseA.Norm(Matrix.NumP.Infinity);
            Console.WriteLine(q1);
            Console.WriteLine();
            Console.WriteLine("Теоретическая относительная погрешность:");
            Console.WriteLine("q(A)*sigmaB =");
            Console.WriteLine();
            Console.WriteLine("(p = 1)");
            double sigmaQ = q * (deltaB.Norm(Vector.NumP.One) / b.Norm(Vector.NumP.One));
            Console.WriteLine(sigmaQ);
            Console.WriteLine();
            Console.WriteLine("(p = infinity)");
            double sigmaQ1 = q1 * (deltaB.Norm(Vector.NumP.Infinity) / b.Norm(Vector.NumP.Infinity));
            Console.WriteLine(sigmaQ1);
            Console.ReadKey();
        }
    }
}
