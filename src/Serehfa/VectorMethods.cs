using System;
using System.Collections.Generic;

namespace Serehfa
{
    using static ArgHelpers;

    /// <summary>
    ///  Methods to interact with Scheme vectors. Our scheme vector
    ///  implementation uses the .NET <see cref="List" />.
    /// </summary>
    public static class VectorMethods
    {
        [LispBuiltin("vector-length")]
        public static object VectorLength(object[] args)
        {
            var vec = UnpackArgs<List<object>>(args);
            return (Double)vec.Count;
        }

        [LispBuiltin("vector-set!")]
        public static object VectorSet(object[] args)
        {
            var (vec, index, value) = UnpackArgs<List<object>,Double,object>(args);
            vec[(int)index] = value;
            return null;
        }

        [LispBuiltin("vector-ref")]
        public static object VectorRef(object[] args)
        {
            var (vec, index) = UnpackArgs<List<object>, Double>(args);
            return vec[(int)index];
        }
        
        [LispBuiltin("make-vector")]
        public static object MakeVector(object[] args)
        {
            var (size, init) = UnpackArgs<Double, object>(args);
            var isize = (int)size;
            var vec = new List<object>(isize);
            for (int i = 0; i < isize; i++)
            {
                vec.Add(init);
            }
            return vec;
        }
    }
}