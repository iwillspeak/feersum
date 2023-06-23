using Serehfa.Attributes;

namespace Serehfa
{
    using static ArgHelpers;

    [LispLibrary("feersum", "serehfa", "symbols")]
    public static class Symbols
    {
        [LispExport("symbol?")]
        public static object IsSymbol(object[] args)
        {
            return UnpackArgs<object>(args) is Ident;
        }

        [LispExport("symbol=?")]
        public static object SymbolEquals(object[] args)
        {
            CheckAtLeastArgs(args, 2);
            if (args[0] is not Ident comparand)
            {
                return false;
            }
            for (int i = 1; i < args.Length; i++)
            {
                if (!(args[i] is Ident id && id == comparand))
                {
                    return false;
                }
            }
            return true;
        }

        [LispExport("symbol->string")]
        public static object SymbolToString(object[] args)
        {
            return UnpackArgs<Ident>(args).Raw;
        }

        [LispExport("string->symbol")]
        public static object StringToSymbol(object[] args)
        {
            return new Ident(UnpackArgs<string>(args));
        }
    }
}
