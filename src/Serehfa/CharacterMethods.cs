namespace Serehfa
{
    using System;
    using static ArgHelpers;

    public static class CharacterMethods
    {
        [LispBuiltin("char?")]
        public static object IsChar(object[] args)
        {
            return UnpackArgs<object>(args) is char;
        }

        [LispBuiltin("digit-value")]
        public static object DigitValue(object[] args)
        {
            var c = UnpackArgs<char>(args);
            if (!char.IsDigit(c))
            {
                return false;
            }
            return char.GetNumericValue(c);
        }
    }
}