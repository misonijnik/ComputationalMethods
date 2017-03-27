using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LinearAlgebra.Helpers;
using LinearAlgebra.Vectors;

namespace LinearAlgebra.Matrix
{
    public class Matrix
    {
        private double[][] _matrix;

        public int Dimension { get; private set; }

        public static Matrix Create(IEnumerable<double> values)
        {
            Check.NotNull(values);
            return new Matrix(values.ToArray());
        }

        public static Matrix CreateIdentity(int dimension)
        {
            Matrix identity = Matrix.Create(new double[dimension, dimension]);
            for (int k = 1; k <= dimension; k++)
            {
                identity[k, k] = 1;
            }
            return identity;
        }

        public Matrix Clone()
        {
            return Create(_matrix);
        }

        private Matrix(double[] sequence)
        {
            int dimension;
            if (!int.TryParse(Math.Sqrt(sequence.Length).ToString(CultureInfo.InvariantCulture), out dimension))
            {
                throw new ArgumentException("Wrong number of elements.");
            }
            Dimension = dimension;
            _matrix = new double[Dimension][];
            for (int i = 0; i < Dimension; i++)
            {
                _matrix[i] = new double[Dimension];
                for (int j = 0; j < Dimension; j++)
                {
                    _matrix[i][j] = sequence[j + i*Dimension];
                }
            }
        }

        public double this[int indexI, int indexJ]
        {
            get
            {
                Check.InDiapason(indexI, $"Index must be greater than zero and less than or equal {Dimension}",
                    1, Dimension);
                Check.InDiapason(indexJ, $"Index must be greater than zero and less than or equal {Dimension}",
                    1, Dimension);
                return _matrix[indexI - 1][indexJ - 1];
            }

            set
            {
                Check.InDiapason(indexI, $"Index must be greater than zero and less than or equal {Dimension}",
                    1, Dimension);
                Check.InDiapason(indexJ, $"Index must be greater than zero and less than or equal {Dimension}",
                    1, Dimension);
                _matrix[indexI - 1][indexJ - 1] = value;
            }
        }

        private int MaxNumberOfChars(IEnumerable<string> strings)
        {
            return strings.Select(str => str.Length).Max();
        }

        private int[] MaxLengths()
        {
            int[] result = new int[Dimension];
            for (int j = 0; j < Dimension; j++)
            {
                double[] tmpArray = new double[Dimension];
                for (int i = 0; i < Dimension; i++)
                {
                    tmpArray[i] = _matrix[i][j];
                }
                result[j] = MaxNumberOfChars(tmpArray.Select(x => x.ToString(CultureInfo.InvariantCulture)));
            }
            return result;
        }

        public Matrix Transposition()
        {
            double[,] matrix = new double[Dimension, Dimension];
            for (int i = 0; i < Dimension; i++)
            {
                for (int j = 0; j < Dimension; j++)
                {
                    
                    matrix[i,j] = _matrix[j][i];
                }
            }
            return Create(matrix);
        }

        public static Matrix Sum(Matrix left, Matrix right)
        {
            return Create(left._matrix.Zip(right._matrix, (x, y) => x.Zip(y, (u, v) => u + v).ToArray()).ToArray());
        }

        public static Matrix Substraction(Matrix left, Matrix right)
        {
            return Sum(left, MultiplyOnScalar(-1, right));
        }

        public static Matrix MultiplyOnScalar(double scalar, Matrix matrix)
        {
            return Create(matrix._matrix.Select(x => x.Select(y => y*scalar).ToArray()).ToArray());
        }

        public static Vector MultiplyOnVector(Matrix matrix, Vector vector)
        {
            if (matrix.Dimension != vector.Dimension)
            {
                throw new ArgumentException();
            }
            return Vector.Create(matrix._matrix.Select(x => Vector.ScalarProduct(Vector.Create(x), vector)));
        }

        public static Matrix Create(double[,] matrix)
        {
            List<double> tmp = new List<double>();
            foreach (double d in matrix)
            {
                tmp.Add(d);
            }
            return Create(tmp);
        }

        public static Matrix Create(double[][] matrix)
        {
            List<double> tmp = new List<double>();
            foreach (var col in matrix)
            {
                foreach (var d in col)
                {
                    tmp.Add(d);
                }
            }
            return Create(tmp);
        }

        private string Fill(string str, int length)
        {
            int diff = length - str.Length;
            if (diff > 0)
            {
                for (int i = 0; i < diff; i++)
                {
                    str += " ";
                }
            }
            return str;
        }

        public enum NumP
        {
            One,
            Infinity
        }

        public double Norm(NumP number)
        {
            double result = 0;
            switch (number)
            {
                case NumP.One:
                    double[][] matrix = new double[Dimension][];
                    for (int i = 0; i < Dimension; i++)
                    {
                        matrix[i] = new double[Dimension];
                        for (int j = 0; j < Dimension; j++)
                        {

                            matrix[i][j] = _matrix[j][i];
                        }
                    }
                    result = matrix.Select(x => x.Select(Math.Abs).Sum()).Max();
                    break;
                case NumP.Infinity:
                    result = _matrix.Select(x => x.Select(Math.Abs).Sum()).Max();
                    break;
            }
            return result;
        }

        public override string ToString()
        {
            string result = "";
            int[] maxNumbers = MaxLengths();
            result += new string('-', maxNumbers.Sum() + Dimension - 1) + "\n";
            string[][] tmpStrings = new string[Dimension][];
            for (int i = 0; i < Dimension; i++)
            {
                tmpStrings[i] = new string[Dimension];
                for (int j = 0; j < Dimension; j++)
                {
                    tmpStrings[i][j] = Fill(_matrix[i][j].ToString(CultureInfo.InvariantCulture), maxNumbers[j]);
                }
                result += string.Join("|", tmpStrings[i]) + "\n";
            }
            result += new string('-', maxNumbers.Sum() + Dimension - 1);
            return result;
        }
    }
}
