using System;
using System.Text;

using Serehfa.Attributes;

namespace Serehfa;

using static ArgHelpers;

[LispLibrary("feersum", "serehfa", "strings")]
public static class Strings
{
    [LispExport("string=?")]
    public static object StringEquals(object[] args)
    {
        CheckAtLeastArgs(args, 2);
        if (args[0] is not string comparand)
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

    [LispExport("string->number")]
    public static object StringToNumber(object[] args)
    {
        if (args.Length == 1)
        {
            return double.Parse(UnpackArgs<string>(args));
        }

        var (input, bae) = UnpackArgs<string, double>(args);
        return (double)Convert.ToUInt64(input, (int)bae);
    }

    [LispExport("string-append")]
    public static object StringAppend(object[] args)
    {
        CheckAtLeastArgs(args, 1);

        var sb = new StringBuilder(Unpack<string>(args[0]));
        for (int i = 1; i < args.Length; i++)
        {
            var toAppend = Unpack<string>(args[i]);
            sb.Append(toAppend);
        }

        return sb.ToString();
    }
}
