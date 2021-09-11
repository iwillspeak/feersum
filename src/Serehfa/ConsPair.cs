using System.Text;

namespace Serehfa
{
    public class ConsPair
    {
        private object _car;
        private object _cdr;

        public ConsPair(object car, object cdr)
        {
            _car = car;
            _cdr = cdr;
        }

        public object Car => _car;
        public object Cdr => _cdr;

        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append("(");
            var current = this;
            while (current != null)
            {
                sb.Append(Write.GetExternalRepresentation(current._car));
                if (current._cdr is ConsPair tail)
                {
                    sb.Append(" ");
                    current = tail;
                }
                else if (current._cdr is null)
                {
                    current = null;
                }
                else
                {
                    sb.Append(" . ");
                    sb.Append(current._cdr.ToString());
                    break;
                }
            }
            sb.Append(")");
            return sb.ToString();
        }
    }
}
