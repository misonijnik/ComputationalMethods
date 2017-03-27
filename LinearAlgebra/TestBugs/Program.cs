using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SystemLinearEquation;
using LinearAlgebra.Matrix;
using LinearAlgebra.Vectors;

namespace TestBugs
{
    class Program
    {
        static void Main(string[] args)
        {
           Matrix system = Matrix.Create(new[,]
           {
                {6.5176E-06, -8.0648E-03, 4.23528 },
                {5.9176E-03, -0.80648,    1.46528 },
                {0.87176,     0.79352,    0.91528}
            });
            Vector ed = Vector.Create(new[] { 0, 1.0, 0 });
            Vector bt = Matrix.MultiplyOnVector(system, ed);
            Vector result = Solver.GaussWithChoiceSolveSystem(system, bt);
            Console.WriteLine(result);
            Console.ReadKey();
        }
    }
}
