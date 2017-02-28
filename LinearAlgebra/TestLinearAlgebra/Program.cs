using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace TestLinearAlgebra
{
    class Program
    {
        static void Main(string[] args)
        {
            double test = 5;
            int tes = Convert.ToInt32(test);
            Console.WriteLine(tes);
            var a = Vector.Create(new double[] {2, 35, 4});
            var b = Vector.Create(new double[] {1, 1, 1});
            var c = Vector.Sum(a, b);
            Console.WriteLine(c);
            var d = Vector.Create(new double[] {4, 5 ,6});
            d[3] = 7;
            Console.WriteLine(d[1]);
            var prod = Vector.ScalarProduct(a, b);
            Console.WriteLine(prod);



            Matrix mt = Matrix.Create(new double[,]
            {
                {1, 2.5, 3},
                {3, 4,   5},
                {5, 6,   70}
            });
            Console.WriteLine(mt);
            Console.WriteLine(mt.Norm(Matrix.NumP.Infinity));
            Console.WriteLine(Matrix.MultiplyOnVector(mt,d));
            Console.ReadKey();
        }
    }
}
