using System;
using System.Linq;
using System.Text;

namespace Serehfa
{
    public class Ident
    {
        private readonly string _id;

        public Ident(string id)
        {
            _id = string.Intern(id);
        }

        public bool IsSimple => _id.All(c => char.IsLetterOrDigit(c));

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
}
