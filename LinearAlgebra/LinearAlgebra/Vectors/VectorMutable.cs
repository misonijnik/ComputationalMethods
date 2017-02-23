using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LinearAlgebra.Vectors
{
    public class VectorMutable : Vector, ICreator<VectorMutable>
    {
        protected internal VectorMutable(double[] values) : base(values)
        {
        }

        public double this[int index]
        {
            set
            {
                Check.InDiapason(index, $"Index must be greater than zero and less than or equal {Dimension}",
                    1, Dimension);
                vector[index] = value;
            }

            get
            {
                Check.InDiapason(index, $"Index must be greater than zero and less than or equal {Dimension}",
                    1, Dimension);
                return vector[index];
            }
        }

        VectorMutable ICreator<VectorMutable>.Create<T>(T values)
        {
            return CreateMutable(values);
        }

    }
}
