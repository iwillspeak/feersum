namespace Serehfa
{
    using System;
    using static ArgHelpers;

    public static class Write
    {
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

        public static string GetDisplayRepresentation(object o) => o switch
        {
            bool b => b ? "#t" : "#f",
            double d => d.ToString("G"),
            char c => char.IsLetterOrDigit(c) ?
                @"#\" + c : $@"#\x{Convert.ToUInt32(c):x4}",
            null => "'()",
            Func<object[], object> f => $"#<compiledProcedure {f.Method}>",
            object[] v => VectorMethods.GetDisplayRepresentation(v),
            byte[] v => ByteVectorMethods.GetDisplayRepresentation(v),
            _ => o.ToString(),
        };
    }
}
