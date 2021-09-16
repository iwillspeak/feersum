using Serehfa.Attributes;

namespace Serehfa
{
    using static ArgHelpers;

    [LispLibrary("scheme", "base")]
    public static class Strings
    {
        [LispBuiltin("string=?")]
        public static object StringEquals(object[] args)
        {
            CheckAtLeastArgs(args, 2);
            if (!(args[0] is string comparand))
            {
                return false;
            }
            for (int i = 1; i < args.Length; i++)
            {
                if (!(args[i] is string s && s == comparand))
                {
                    return false;
                }
            }
            return true;
        }
    }
}
