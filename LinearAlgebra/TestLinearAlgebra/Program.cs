using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;
using SystemLinearEquation;

namespace TestLinearAlgebra
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Матрица системы:");
            Console.WriteLine("A = ");
            Matrix system = Matrix.Create(new [,]
            {
                {6.5176E-06, -8.0648E-03, 4.23528 },
                {5.9176E-03, -0.80648,    1.46528 },
                {0.87176,     0.79352,    0.91528}
            });
            Console.WriteLine(system);
            Console.WriteLine("b = ");
            Vector systemVec = Vector.Create(new []
            {
                 3.61628, 1.52097, 1.81150
            });
            Console.WriteLine(systemVec);
            
            Console.WriteLine("Решение обычным методом Гаусса:");
            Vector result = Solver.GaussSolveSystem(system, systemVec);
            Console.WriteLine(result);
            Console.WriteLine("Невязка:");
            Vector offset = Vector.Substraction(Matrix.MultiplyOnVector(system, result), systemVec);
            Console.WriteLine(offset);

            Console.WriteLine("Решение методом Гаусса с выбором главного элемента по строке:");
            result = Solver.GaussWithChoiceSolveSystem(system, systemVec);
            Console.WriteLine(result);
            Console.WriteLine("Невязка:");
            offset = Vector.Substraction(Matrix.MultiplyOnVector(system, result), systemVec);
            Console.WriteLine(offset);

            Console.WriteLine("Определитель:");
            Console.WriteLine(Solver.Determinant(system));
            Console.WriteLine("Обратная матрица:");
            Matrix inverse = Solver.Inverse(system);
            Console.WriteLine(inverse);

            Console.WriteLine("Решение с использованием обратной матрицы:");
            result = Matrix.MultiplyOnVector(inverse, systemVec);
            Console.WriteLine(result);
            Console.WriteLine("Невязка:");
            offset = Vector.Substraction(Matrix.MultiplyOnVector(system, result), systemVec);
            Console.WriteLine(offset);

            Console.WriteLine("Число обусловленности матрицы:");
            double ob = system.Norm(Matrix.NumP.Infinity) * inverse.Norm(Matrix.NumP.Infinity);
            Console.WriteLine(ob);


            Console.WriteLine("Метод простой итерации");
            Matrix A = Matrix.Create(new [,]
            { 
                { 10.409187, 1.2494191, -3.2136953 },
                { 1.2494191, 7.9045365, 0.74772162},
				{ -3.2136953, 0.74772162, 6.2719819}
            });
            Vector b = Vector.Create(new [] 
            {
                2.6696963, -6.9807383, 0.1542235
            });

            Console.WriteLine("A = ");
            Console.WriteLine(A);
            Console.WriteLine("b = ");
            Console.WriteLine(b);

            Vector x = Solver.SimpleIteration(A, b);
            Console.WriteLine("Решение:");
            Console.WriteLine(x);
            Vector v = Vector.Substraction(Matrix.MultiplyOnVector(A, x), b);
            Console.WriteLine("Невязка:");
            Console.WriteLine(v);

            Console.WriteLine("Метод Зейделя");

            Console.WriteLine("A = ");
            Console.WriteLine(A);
            Console.WriteLine("b = ");
            Console.WriteLine(b);

            x = Solver.Seidel(A, b);
            Console.WriteLine("Решение:");
            Console.WriteLine(x);
            v = Vector.Substraction(Matrix.MultiplyOnVector(A, x), b);
            Console.WriteLine("Невязка:");
            Console.WriteLine(v);

            Console.ReadKey();
        }
    }
}
