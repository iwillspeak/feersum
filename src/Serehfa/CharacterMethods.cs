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

        [LispBuiltin("char-alphabetic?")]
        public static object IsAlphabetic(object[] args)
        {
            return char.IsLetter(UnpackArgs<char>(args));
        }

        [LispBuiltin("char-numeric?")]
        public static object IsNumeric(object[] args)
        {
            return char.IsDigit(UnpackArgs<char>(args));
        }

        [LispBuiltin("char-whitespace?")]
        public static object IsWhitespace(object[] args)
        {
            return char.IsWhiteSpace(UnpackArgs<char>(args));
        }

        [LispBuiltin("char-upper-case?")]
        public static object IsUpper(object[] args)
        {
            return char.IsUpper(UnpackArgs<char>(args));
        }

        [LispBuiltin("char-lower-case?")]
        public static object IsLower(object[] args)
        {
            return char.IsLower(UnpackArgs<char>(args));
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

        [LispBuiltin("char->integer")]
        public static object CharToNumber(object[] args)
        {
            return (double)(Convert.ToUInt32(UnpackArgs<char>(args)));
        }

        [LispBuiltin("integer->char")]
        public static object NumberToChar(object[] args)
        {
            try
            {
                if (char.TryParse( 
                    char.ConvertFromUtf32(
                        (int)UnpackArgs<double>(args)), out var res))
                {
                    return res;
                }
            }
            catch (ArgumentOutOfRangeException)
            {
                // If the character can't be parsed ignore that and fall through
                // to the error return.
            }
            return (double)(0x10FFFF + 1);
        }

        [LispBuiltin("char-upcase")]
        public static object CharUpcase(object[] args)
        {
            return char.ToUpperInvariant(UnpackArgs<char>(args));
        }

        [LispBuiltin("char-downcase")]
        public static object CharDowncase(object[] args)
        {
            return char.ToLowerInvariant(UnpackArgs<char>(args));
        }

        [LispBuiltin("char-foldcase")]
        public static object CharFoldcase(object[] args)
        {
            // Is this OK?
            return CharDowncase(args);
        }
    }
}