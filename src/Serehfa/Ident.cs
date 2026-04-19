using System;
using System.Linq;
using System.Text;

namespace Serehfa;

public class Ident
{
    private readonly string _id;

    public Ident(string id)
    {
        _id = string.Intern(id);
    }

    // R7RS 7.1.1 identifier grammar — a symbol can be printed without vertical-bar
    // quoting when it is either a peculiar identifier (+, -, ...) or a regular
    // identifier whose first character is an <initial> and all subsequent characters
    // are <subsequent>.
    //
    //   <initial>    -> letter | ! $ % & * / : < = > ? ^ _ ~
    //   <subsequent> -> <initial> | digit | + - . @
    //
    // Starting with a digit would make the printed form look like a number.
    // Starting with # would look like a boolean / vector literal.
    // The peculiar identifiers that start with +/- followed by sign-subsequent
    // characters are intentionally excluded here; they are rare and the conservative
    // fallback (vertical-bar quoting) is always safe.
    public bool IsSimple
    {
        get
        {
            if (_id.Length == 0) return false;

            // Peculiar identifiers
            if (_id == "+" || _id == "-" || _id == "...") return true;

            // Regular: <initial> <subsequent>*
            if (!IsInitial(_id[0])) return false;
            for (int i = 1; i < _id.Length; i++)
                if (!IsSubsequent(_id[i])) return false;
            return true;
        }
    }

    // <initial> = letter | ! $ % & * / : < = > ? ^ _ ~
    private static bool IsInitial(char c) =>
        char.IsLetter(c) || "!$%&*/:<=>?^_~".IndexOf(c) >= 0;

    // <subsequent> = <initial> | digit | + - . @
    private static bool IsSubsequent(char c) =>
        IsInitial(c) || char.IsDigit(c) || c == '+' || c == '-' || c == '.' || c == '@';

    public string Raw => _id;

    public override int GetHashCode() => (_id).GetHashCode();

    public override bool Equals(object obj) => obj switch
    {
        Ident other => _id == other._id,
        _ => false,
    };

    public override string ToString() => IsSimple ?
        _id : Quote(_id);

    private static string Quote(string id)
    {
        var sb = new StringBuilder();
        sb.Append("|");
        foreach (var c in id)
        {
            switch (c)
            {
                case '\\':
                case '|':
                    sb.AppendFormat("\\{0}", c);
                    break;
                case ' ': sb.Append(' '); break;
                default:
                    if (char.IsWhiteSpace(c))
                    {
                        sb.AppendFormat("\\x{0:X};", (int)c);
                    }
                    else
                    {
                        sb.Append(c);
                    }
                    break;
            }
        }
        sb.Append("|");
        return sb.ToString();
    }

    public static bool operator ==(Ident lhs, Ident rhs)
    {
        return lhs._id == rhs._id;
    }

    public static bool operator !=(Ident lhs, Ident rhs)
    {
        return lhs._id != rhs._id;
    }
}
