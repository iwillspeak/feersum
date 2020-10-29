using System;
using System.Text;

namespace Serehfa
{
    using static ArgHelpers;
    
    public static class ByteVectorMethods
    {
        [LispBuiltin("bytevector?")]
        public static object IsBytevector(object[] args)
        {
            var maybeVec = UnpackArgs<object>(args);

            return maybeVec is byte[];
        }

        [LispBuiltin("make-bytevector")]
        public static object MakeBytevector(object[] args)
        {
            // TODO: Narrow down the types here when we support numerics better.
            var (size, fill) = UnpackArgs<double, double>(args);
            var byteFill = (byte)fill;

            var vec = new byte[(int)size];
            for (var i = 0; i < (int)size; i++)
            {
                vec[i] = byteFill;
            }
            
            return vec;
        }

        [LispBuiltin("bytevector")]
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

        internal static string GetDisplayRepresentation(byte[] bytes)
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