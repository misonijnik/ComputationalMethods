using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LinearAlgebra.Vectors
{
    public interface ICreator<out T>
    {
        T Create<V>(V values)
            where V : IEnumerable<double>;
    }
}
