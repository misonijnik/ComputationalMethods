using System.Collections.Generic;

namespace LinearAlgebra.Helpers
{
    public interface ICreator<out T>
    {
        T Create<V>(V values)
            where V : IEnumerable<double>;
    }
}
