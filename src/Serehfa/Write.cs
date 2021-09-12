namespace Serehfa
{
    using System;
    using System.Text;
    using static ArgHelpers;

    public static class Write
    {
        /// <summary>
        /// The newline method. This is a prime candidate for being moved to
        /// a pure scheme definition.
        /// </summary>
        [LispBuiltin("newline")]
        public static object Newline(object[] args)
        {
            CheckNoArgs(args);

            Console.WriteLine();

            return Undefined.Instance;
        }

        /// <summary>
        /// Display builtin. This is intended for user-readable output rather than
        /// any machine readable round tripping. Printing out strings &amp; chars should
        /// display their raw form. All other objects is up to the implementation.
        /// 
        /// This implementation calls `ToString` on the underlying .NET object and
        /// uses that directly.
        /// </summary>
        [LispBuiltin("display")]
        public static object Display(object[] args)
        {
            var obj = UnpackArgs<object>(args);

            var repr = GetDisplayRepresentation(obj);

            Console.Write(repr);

            return Undefined.Instance;
        }

        /// <summary>
        /// Write builtin. This is intended for round-trippable and machine
        /// readable output.
        /// </summary>
        [LispBuiltin("write")]
        public static object WriteExternal(object[] args)
        {
            var obj = UnpackArgs<object>(args);

            var repr = GetExternalRepresentation(obj);

            Console.Write(repr);

            return Undefined.Instance;
        }

        public static string GetDisplayRepresentation(object o) => o switch
        {
            string s => s,
            char c => c.ToString(),
            _ => GetExternalRepresentation(o),
        };

        public static string GetExternalRepresentation(object o)  => o switch
        {
            bool b => b ? "#t" : "#f",
            double d => d.ToString("G"),
            string s => EscapeString(s),
            char c => char.IsLetterOrDigit(c) || char.IsPunctuation(c) ?
                @"#\" + c : $@"#\x{Convert.ToUInt32(c):x4}",
            null => "'()",
            Func<object[], object> f => $"#<compiledProcedure {f.Method}>",
            object[] v => VectorMethods.GetExternalRepresentation(v),
            byte[] v => ByteVectorMethods.GetExternalRepresentation(v),
            _ => o.ToString(),
        };

        private static string EscapeString(string s)
        {
            var sb = new StringBuilder();
            sb.Append("\"");
            foreach (var c in s)
            {
                if (c == '\\' || c == '\"')
                {
                    sb.AppendFormat(@"\{0}", c);
                }
                else if (char.IsLetterOrDigit(c) || char.IsPunctuation(c) || c == ' ')
                {
                    sb.Append(c);
                }
                else
                {
                    sb.AppendFormat(@"\x{0:x4};",Convert.ToUInt32(c));
                }
            }
            sb.Append("\"");
            return sb.ToString();
        }
    }
}
