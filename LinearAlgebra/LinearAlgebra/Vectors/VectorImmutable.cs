using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LinearAlgebra.Vectors
{
    public class VectorImmutable : Vector, ICreator<VectorImmutable>
    {
        protected internal VectorImmutable(double[] values) : base(values)
        {
        }

        public double this[int index]
        {
            get
            {
                Check.InDiapason(index, $"Index must be geater than zero and less than or equal {Dimension}",
                    1, Dimension);
                return vector[index];
            }
        }

        VectorImmutable ICreator<VectorImmutable>.Create<T>(T values)
        {
            return CreateImmutable(values);
        }
    }
}
