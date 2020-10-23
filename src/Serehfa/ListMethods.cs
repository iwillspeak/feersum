namespace Serehfa
{
    using static ArgHelpers;

    public class ListMethods
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
            return toTest is ConsPair pair && (pair.Cdr == null || pair.Cdr is ConsPair);
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
    }
}