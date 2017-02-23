using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Vectors;

namespace TestLinearAlgebra
{
    class Program
    {
        static void Main(string[] args)
        {
            var a = Vector.CreateImmutable(new double[] {2, 3, 4});
            var b = Vector.CreateImmutable(new double[] {1, 1, 1});
            var c = Vector.Sum(a, b);
            Console.WriteLine(c);
            var d = Vector.CreateMutableZero(3);
            d[1] = 5;
            Console.WriteLine(d);
            Console.ReadKey();
        }
    }
}
