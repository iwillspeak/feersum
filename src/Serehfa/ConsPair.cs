using System.Text;

namespace Serehfa
{
    public class ConsPair
    {
        public ConsPair(object car, object cdr)
        {
            Car = car;
            Cdr = cdr;
        }

        public object Car { get; set; }
        public object Cdr { get; set; }

        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append("(");
            var current = this;
            while (current != null)
            {
                sb.Append(Write.GetExternalRepresentation(current.Car));
                if (current.Cdr is ConsPair tail)
                {
                    sb.Append(" ");
                    current = tail;
                }
                else if (current.Cdr is null)
                {
                    current = null;
                }
                else
                {
                    sb.Append(" . ");
                    sb.Append(Write.GetExternalRepresentation(current.Cdr));
                    break;
                }
            }
            sb.Append(")");
            return sb.ToString();
        }
    }
}
