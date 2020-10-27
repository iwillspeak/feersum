using System;
using System.Runtime.CompilerServices;

namespace Serehfa
{
    using static ArgHelpers;

    public static class Arithmetics
    {
        [LispBuiltin("+")]
        public static object Arithadd(object[] args)
        {
            return ArithOp(args, 0.0, (a, b) => a + b);
        }

        [LispBuiltin("-")]
        public static object Arithsub(object[] args)
        {
            return ArithOp(args, 0.0, (a, b) => a - b);
        }

        [LispBuiltin("/")]
        public static object Arithdiv(object[] args)
        {
            return ArithOp(args, 1.0, (a, b) => a / b);
        }

        [LispBuiltin("*")]
        public static object Arithmul(object[] args)
        {
            return ArithOp(args, 1.0, (a, b) => a * b);
        }

        [LispBuiltin("=")]
        public static object Aritheq(object[] args)
        {
            return ComparisonOp(args, (a, b) => a == b);
        }

        [LispBuiltin(">")]
        public static object Arithgt(object[] args)
        {
            return ComparisonOp(args, (a, b) => a > b);
        }

        [LispBuiltin("<")]
        public static object Arithlt(object[] args)
        {
            return ComparisonOp(args, (a, b) => a < b);
        }

        [LispBuiltin(">=")]
        public static object Arithgte(object[] args)
        {
            return ComparisonOp(args, (a, b) => a >= b);
        }

        [LispBuiltin("<=")]
        public static object Arithlte(object[] args)
        {
            return ComparisonOp(args, (a, b) => a <= b);
        }

        /// <summary>
        /// Arithmetic builtin. Combines all elements in the arguments array with
        /// the given <paramref name="op"/>. If only 1 element is provided then
        /// <paramref name="def"/> is used to compute some form of inverse. If no
        /// arguments are provided then <paramref name="def" /> is returned.
        /// </summary>
        /// <param name="args">The arguments to be combined.</param>
        /// <param name="def">The default value to use as a seed.</param>
        /// <param name="op">The operator to combine elements with</param>
        /// <returns>An arithmetic combination of the arguments.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static object ArithOp(
            object[] args,
            double def,
            Func<double, double, double> op)
        {
            double ret;
            int i;

            if (args.Length < 2)
            {
                ret = def;
                i = 0;
            }
            else
            {
                ret = Unpack<double>(args[0]);
                i = 1;
            }

            while (i < args.Length)
            {
                ret = op(ret, Unpack<double>(args[i++]));
            }

            return ret;
        }

        /// <summary>
        /// Comparator Builtin. Builds a method which uses the given
        /// <paramref name="comarator" /> compare all arguments and returns a
        /// boolean result. If 0 or 1 elements are  given the method will always
        /// return <c>#t</c>.
        /// </summary>
        /// <param name="args">The arguments to compare</param>
        /// <param name="comparator">The comparison opertor to use.</param>
        /// <returns>
        /// True if the sequence progresses with the operator, false otherwise
        /// </returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static object ComparisonOp(
            object[] args, Func<double, double, bool> comparator)
        {
            if (args.Length < 2)
            {
                return true;
            }

            var last = Unpack<double>(args[0]);

            for (int i = 1; i < args.Length; i++)
            {
                var cur = Unpack<double>(args[i]);
                if (!comparator(last, cur))
                {
                    return false;
                }
                last = cur;
            }
            return true;
        }
    }
}