namespace Serehfa
{
    using static ArgHelpers;

    public static class SymbolMethods
    {
        [LispBuiltin("symbol?")]
        public static object IsSymbol(object[] args)
        {
            return UnpackArgs<object>(args) is Ident;
        }

        [LispBuiltin("symbol=?")]
        public static object SymbolEquals(object[] args)
        {
            CheckAtLeastArgs(args, 2);
            if (!(args[0] is Ident comparand))
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

        [LispBuiltin("symbol->string")]
        public static object SymbolToString(object[] args)
        {
            return UnpackArgs<Ident>(args).Raw;
        }

        [LispBuiltin("string->symbol")]
        public static object StringToSymbol(object[] args)
        {
            return new Ident(UnpackArgs<string>(args));
        }
    }
}
