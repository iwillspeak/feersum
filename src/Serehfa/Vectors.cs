using System;
using System.Collections.Generic;
using System.Text;
using Serehfa.Attributes;

namespace Serehfa
{
    using static ArgHelpers;

    /// <summary>
    ///  Methods to interact with Scheme vectors. Our scheme vector
    ///  implementation uses the .NET <see cref="List{T}" />.
    /// </summary>
    [LispLibrary("feersum", "sehehfa", "vectors")]
    public static class Vectors
    {
        [LispExport("vector")]
        public static object VectorNew(object[] args)
        {
            return args;
        }

        [LispExport("vector?")]
        public static object IsVector(object[] args)
        {
            var vec = UnpackArgs<object>(args);
            return vec is object[];
        }

        [LispExport("vector-length")]
        public static object VectorLength(object[] args)
        {
            var vec = UnpackArgs<object[]>(args);
            return (Double)vec.Length;
        }

        [LispExport("vector-set!")]
        public static object VectorSet(object[] args)
        {
            var (vec, index, value) = UnpackArgs<object[], Double, object>(args);
            vec[(int)index] = value;
            return null;
        }

        [LispExport("vector-ref")]
        public static object VectorRef(object[] args)
        {
            var (vec, index) = UnpackArgs<object[], Double>(args);
            return vec[(int)index];
        }

        [LispExport("make-vector")]
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

        internal static string GetExternalRepresentation(object[] v)
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
                sb.Append(Write.GetExternalRepresentation(o));
                first = false;
            }

            sb.Append(")");
            return sb.ToString();
        }
    }
}
