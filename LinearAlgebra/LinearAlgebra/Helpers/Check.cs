using System;

namespace LinearAlgebra.Helpers
{
    public static class Check
    {
        public static void NotNull(this object obj)
        {
            if (obj == null)
            {
                throw new ArgumentNullException(nameof(obj));
            }
        }

        public static void InDiapason(double value,string message, double left = double.MinValue, 
            double right = double.MaxValue)
        {
            if (!(value >= left && value <= right))
            {
                throw new ArgumentException(message);
            }
        }

        public static void NaturalNumber(int number, string message)
        {
            InDiapason(value: number, message: message, left: 1);
        }
    }
}
