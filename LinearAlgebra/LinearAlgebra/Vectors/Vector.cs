using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Runtime.Remoting;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Helpers;

namespace LinearAlgebra.Vectors
{
    public class Vector
    {
        private readonly double[] _vector;
        public int Dimension
        {
            get;
        }

        public double[] ToArray()
        {
            return _vector.Clone() as double[];
        }

        public Vector Clone()
        {
            return Vector.Create(_vector);
        }

        private Vector(double[] values)
        {
            _vector = values.Clone() as double[];
            Dimension = _vector.Length;
        }

        public double this[int index]
        {
            set
            {
                Check.InDiapason(index, $"Index must be greater than zero and less than or equal {Dimension}",
                    1, Dimension);
                _vector[index - 1] = value;
            }

            get
            {
                Check.InDiapason(index, $"Index must be greater than zero and less than or equal {Dimension}",
                    1, Dimension);
                return _vector[index - 1];
            }
        }

        /// <typeparam name="T"></typeparam>
        /// <param name="values">
        /// Must not be empty.
        /// </param>
        public static Vector Create(IEnumerable<double> values)
        {
            Check.NotNull(values);

            if (values.Count().Equals(0))
            {
                throw new ArgumentException("The sequence must not be empty.");
            }

            return new Vector(values.ToArray());
        }

        /// <summary>
        /// Create vector with zero values.
        /// </summary>
        /// <param name="dimension">
        /// Must be greater than zero.
        /// </param>
        public static Vector CreateZero(int dimension)
        {
            Check.NaturalNumber(dimension, "Dimension must be greater than zero.");
            return new Vector(new double[dimension]);
        }

        public static Vector Sum(Vector left, Vector right)
        {
            if (!left.Dimension.Equals(right.Dimension))
            {
                throw new ArgumentException("Vectors should have the same dimension");
            }
            return Create(left._vector.Zip(right._vector, (l, r) => l + r));
        }

        public static Vector MultiplyOnScalar(double scalar, Vector vector)
        {
            return Create(vector._vector.Select(x => x * scalar));
        }

        public static Vector Substraction(Vector left, Vector right)
        {
            return Sum(left, MultiplyOnScalar(-1, right));
        }

        public static double ScalarProduct(Vector left, Vector right)
        {
            return left._vector.Zip(right._vector, (l, r) => l * r).Sum();
        }

        private int MaxNumberOfChars(IEnumerable<string> strings)
        {
            return strings.Select(str => str.Length).Max();
        }

        public override string ToString()
        {
            var vectorString = _vector.Select(num => num.ToString(CultureInfo.InvariantCulture));
            int max = MaxNumberOfChars(vectorString);
            string result = new string('-', max);
            result += "\n" + string.Join("\n", _vector.Select(num => num.ToString(CultureInfo.InvariantCulture))) + "\n";
            result += new string('-', max);
            return result;
        }

        public enum NumP
        {
            One,
            Two,
            Infinity
        }

        public double Norm(NumP number)
        {
            double result = 0;
            switch (number)
            {
                case NumP.One:
                    result = _vector.Select(x => Math.Abs(x)).Sum();
                    break;
                case NumP.Two:
                    result = Math.Sqrt(_vector.Select(x => x * x).Sum());
                    break;
                case NumP.Infinity:
                    result = _vector.Select(x => Math.Abs(x)).Max();
                    break;
            }
            return result;
        }
    }
}
