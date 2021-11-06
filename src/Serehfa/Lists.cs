using Serehfa.Attributes;

namespace Serehfa
{
    using static ArgHelpers;

    [LispLibrary("scheme", "base")]
    public class Lists
    {
        [LispBuiltin("null?")]
        public static object NullCheck(object[] args)
        {
            return UnpackArgs<object>(args) == null;
        }

        [LispBuiltin("cons")]
        public static object Cons(object[] args)
        {
            var (left, right) = UnpackArgs<object, object>(args);
            return new ConsPair(left, right);
        }

        [LispBuiltin("list")]
        public static object Listnew(object[] args)
        {
            ConsPair ret = null;
            for (int i = args.Length; i > 0; i--)
            {
                ret = new ConsPair(args[i - 1], ret);
            }
            return ret;
        }

        [LispBuiltin("make-list")]
        public static object MakeList(object[] args)
        {
            object fill = null;
            double length;
            if (args.Length == 1)
            {
                length = UnpackArgs<double>(args);
            }
            else
            {
                (length, fill) = UnpackArgs<double, object>(args);
            }

            ConsPair tail = null;

            for (int i = 0; i < length; i++)
            {
                tail = new ConsPair(fill, tail);
            }

            return tail;
        }

        [LispBuiltin("pair?")]
        public static object IsPair(object[] args)
        {
            var toTest = UnpackArgs<object>(args);
            return toTest is ConsPair;
        }

        [LispBuiltin("list?")]
        public static object IsList(object[] args)
        {
            var toTest = UnpackArgs<object>(args);
            return toTest switch
            {
                null => true,
                ConsPair pair => (pair.Cdr == null || pair.Cdr is ConsPair),
                _ => false,
            };
        }

        [LispBuiltin("car")]
        public static object Car(object[] args)
        {
            var list = UnpackArgs<ConsPair>(args);
            return list.Car;
        }

        [LispBuiltin("cdr")]
        public static object Cdr(object[] args)
        {
            var list = UnpackArgs<ConsPair>(args);
            return list.Cdr;
        }

        [LispBuiltin("set-car!")]
        public static object SetCar(object[] args)
        {
            var (list, newCar) = UnpackArgs<ConsPair, object>(args);
            list.Car = newCar;
            return Undefined.Instance;
        }

        [LispBuiltin("set-cdr!")]
        public static object SetCdr(object[] args)
        {
            var (list, newCdr) = UnpackArgs<ConsPair, object>(args);
            list.Cdr = newCdr;
            return Undefined.Instance;
        }

        [LispBuiltin("length")]
        public static object Length(object[] args)
        {
            var list = UnpackArgs<ConsPair>(args);
            var length = 0;
            while (list != null)
            {
                length++;
                if (list.Cdr is ConsPair tail)
                {
                    list = tail;
                }
                else
                {
                    break;
                }
            }
            return (double)length;
        }
    }
}
