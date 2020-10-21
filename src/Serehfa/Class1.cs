using System;

namespace Serehfa
{
    public class Class1
    {
        [LispBuiltin("test")]
        public static object TestMethod(object[] parameters)
        {
            return parameters.Length;
        }
    }
}
