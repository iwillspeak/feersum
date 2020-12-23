using System;
using System.Collections.Generic;
using System.Text;

namespace Serehfa
{
    using static ArgHelpers;

    /// <summary>
    ///  Methods to interact with Scheme vectors. Our scheme vector
    ///  implementation uses the .NET <see cref="List" />.
    /// </summary>
    public static class VectorMethods
    {
        [LispBuiltin("vector")]
        public static object VectorNew(object[] args)
        {
            return args;
        }

        [LispBuiltin("vector?")]
        public static object IsVector(object[] args)
        {
            var vec = UnpackArgs<object>(args);
            return vec is object[];
        }

        [LispBuiltin("vector-length")]
        public static object VectorLength(object[] args)
        {
            var vec = UnpackArgs<object[]>(args);
            return (Double)vec.Length;
        }

        [LispBuiltin("vector-set!")]
        public static object VectorSet(object[] args)
        {
            var (vec, index, value) = UnpackArgs<object[], Double, object>(args);
            vec[(int)index] = value;
            return null;
        }

        [LispBuiltin("vector-ref")]
        public static object VectorRef(object[] args)
        {
            var (vec, index) = UnpackArgs<object[], Double>(args);
            return vec[(int)index];
        }

        [LispBuiltin("make-vector")]
        public static object MakeVector(object[] args)
        {
            var (size, init) = UnpackArgs<Double, object>(args);
            var isize = (int)size;
            var vec = new object[isize];
            for (int i = 0; i < isize; i++)
            {
                vec[i] = init;
            }
            return vec;
        }

        internal static string GetDisplayRepresentation(object[] v)
        {
            var sb = new StringBuilder();
            sb.Append("#(");
            bool first = true;
            foreach (var o in v)
            {
                if (!first)
                {
                    sb.Append(" ");
                }
                sb.Append(Write.GetDisplayRepresentation(o));
                first = false;
            }

            sb.Append(")");
            return sb.ToString();
        }
    }
}
