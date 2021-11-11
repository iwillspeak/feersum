using System;
using System.Text;
using Serehfa.Attributes;

namespace Serehfa
{
    using static ArgHelpers;

    [LispLibrary("feersum", "serehfa", "bytevectors")]
    public static class ByteVectors
    {
        [LispExport("bytevector?")]
        public static object IsBytevector(object[] args)
        {
            var maybeVec = UnpackArgs<object>(args);

            return maybeVec is byte[];
        }

        [LispExport("make-bytevector")]
        public static object MakeBytevector(object[] args)
        {
            // TODO: Narrow down the types here when we support numerics better.
            var (size, fill) =
                args.Length == 1 ?
                    (UnpackArgs<double>(args), default) :
                    UnpackArgs<double, double>(args);
            var byteFill = (byte)fill;

            var vec = new byte[(int)size];

            if (byteFill != default)
            {
                for (var i = 0; i < (int)size; i++)
                {
                    vec[i] = byteFill;
                }
            }

            return vec;
        }

        [LispExport("bytevector-length")]
        public static object BytevectorLength(object[] args)
        {
            var vec = UnpackArgs<byte[]>(args);
            return (Double)vec.Length;
        }

        [LispExport("bytevector-u8-set!")]
        public static object BytevectorSet(object[] args)
        {
            var (vec, index, value) = UnpackArgs<byte[], Double, Double>(args);
            vec[(int)index] = (byte)value;
            return null;
        }

        [LispExport("bytevector-u8-ref")]
        public static object BytevectorRef(object[] args)
        {
            var (vec, index) = UnpackArgs<byte[], Double>(args);
            return (double)vec[(int)index];
        }

        [LispExport("bytevector")]
        public static object NewBytevector(object[] args)
        {
            var vec = new byte[args.Length];

            for (var i = 0; i < args.Length; i++)
            {
                var fill = (byte)Unpack<double>(args[i]);
                vec[i] = fill;
            }

            return vec;
        }

        internal static string GetExternalRepresentation(byte[] bytes)
        {
            var sb = new StringBuilder();
            sb.Append("#u8(");
            bool first = true;
            foreach (var b in bytes)
            {
                if (!first)
                {
                    sb.Append(" ");
                }
                sb.Append(b);
                first = false;
            }

            sb.Append(")");
            return sb.ToString();
        }
    }
}
