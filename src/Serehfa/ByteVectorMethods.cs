using System;
using System.Text;

namespace Serehfa
{
    public static class ByteVectorMethods
    {
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