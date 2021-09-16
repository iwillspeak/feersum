using Serehfa.Attributes;

namespace Serehfa
{
    using static ArgHelpers;

    [LispLibrary("scheme", "base")]
    public static class Booleans
    {
        [LispBuiltin("not")]
        public static object Not(object[] args)
        {
            return UnpackArgs<object>(args) is false;
        }

        [LispBuiltin("boolean?")]
        public static object IsBoolean(object[] args)
        {
            return UnpackArgs<object>(args) is bool;
        }

        [LispBuiltin("boolean=?")]
        public static object BooleanEq(object[] args)
        {
            CheckAtLeastArgs(args, 2);
            if (!(args[0] is bool comparand))
            {
                return false;
            }
            for (int i = 0; i < args.Length; i++)
            {
                if (!(args[i] is bool b && b == comparand))
                {
                    return false;
                }
            }
            return true;
        }
    }
}
