using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
    }
}
