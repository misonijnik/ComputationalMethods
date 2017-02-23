using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Runtime.Remoting;
using System.Text;
using System.Threading.Tasks;

namespace LinearAlgebra.Vectors
{
    public abstract class Vector
    {
        protected readonly double[] vector;
        public readonly int Dimension;

        protected Vector(double[] values)
        {
            vector = values.Clone() as double[];
            Dimension = vector.Length;
        }

        /// <typeparam name="T"></typeparam>
        /// <param name="values">
        /// Must not be empty.
        /// </param>
        public static VectorMutable CreateMutable<T>(T values)
            where T : IEnumerable<double>
        {
            Check.NotNull(values);

            if (values.Count().Equals(0))
            {
                throw new ArgumentException("The sequence must not be empty.");
            }

            return new VectorMutable(values.ToArray());
        }

        /// <typeparam name="T"></typeparam>
        /// <param name="values">
        /// Must not be empty.
        /// </param>
        public static VectorImmutable CreateImmutable<T>(T values)
            where T : IEnumerable<double>
        {
            Check.NotNull(values);

            if (values.Count().Equals(0))
            {
                throw new ArgumentException("The sequence must not be empty.");
            }

            return new VectorImmutable(values.ToArray());
        }

        /// <summary>
        /// Create vector with zero values.
        /// </summary>
        /// <param name="dimension">
        /// Must be greater than zero.
        /// </param>
        public static VectorMutable CreateMutableZero(int dimension)
        {
            Check.NaturalNumber(dimension, "Dimension must be greater than zero.");
            return new VectorMutable(new double[dimension]);
        }

        /// <summary>
        /// Смысл таков: Ограничение на базовый класс позволяет докопаться до массива со значениями вектора
        /// и не позволяет использовать какие-то левые типы. А ограничение на обобщенный интерфейс
        /// такого же типа позволяет создать внутри метода класс нужного нам типа. И вуаля, всё работает.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <returns></returns>
        public static T Sum<T>(T left, T right)
            where T : Vector, ICreator<T>
        {
            if (!left.Dimension.Equals(right.Dimension))
            {
                throw new ArgumentException("Vectors should have the same dimension");
            }
            return left.Create(left.vector.Zip(right.vector, (l, r) => l + r));
        }

        public override string ToString()
        {
            return string.Join(",", vector.Select(num => num.ToString(CultureInfo.InvariantCulture)));
        }
    }
}
