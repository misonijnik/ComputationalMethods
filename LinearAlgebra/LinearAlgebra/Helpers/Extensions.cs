using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Vectors;

namespace LinearAlgebra.Helpers
{
    public static class Extensions
    {
        public static int GetHeight<T>(this T[,] array)
        {
            array.NotNull();

            return array.GetLength(0);
        }

        public static int GetWidth<T>(this T[,] array)
        {
            array.NotNull();

            return array.GetLength(1);
        }

        public static double MaxModule(this Vector vector)
        {
            double max = 0;
            for (int i = 1; i <= vector.Dimension; i++)
            {
                if (Math.Abs(vector[i]) > Math.Abs(max))
                {
                    max = vector[i];
                }
            }
            return max;
        }
    }
}
