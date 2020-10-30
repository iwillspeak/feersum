namespace Serehfa
{
    using System;
    using static ArgHelpers;

    public static class Equivalence
    {
        [LispBuiltin("eqv?")]
        public static object Equiv(object[] args)
        {
            return UnpackArgs<object, object>(args) switch
            {
                (bool a, bool b) => a == b,
                (Ident a, Ident b) => a.Equals(b),
                (double a, double b) => a == b,
                (char a, char b) => a == b,
                (Func<object[], object> a, Func<object[], object> b) =>
                    a.Target == b.Target && a.Method == b.Method,
                (null, null) => true,
                (object lhs, object rhs) => 
                    (lhs != null && rhs != null) &&
                    object.ReferenceEquals(lhs, rhs),
            };
        }
    }
}