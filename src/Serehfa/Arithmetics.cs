using System;
using System.Runtime.CompilerServices;

namespace Serehfa
{
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
                ret = unpackNumeric(args[0]);
                i = 1;
            }

            while (i < args.Length)
            {
                ret = op(ret, unpackNumeric(args[i++]));
            }

            return ret;
            
            static double unpackNumeric(object obj)
            {
                if (obj is double d)
                {
                    return d;
                }
                else
                {
                    throw new InvalidCastException("Non-numeric value passed to arithmetic operation.");
                }
            }
        }
    }
}