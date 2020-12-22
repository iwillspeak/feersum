using System;

namespace Serehfa
{
    public class ArgHelpers
    {
        public static void CheckNoArgs(object[] args)
        {
            if (args.Length != 0)
            {
                throw new ArgumentException(
                    $"Expected no arguments, but received {args.Length}",
                    nameof(args));
            }
        }

        public static void CheckAtLeastArgs(object[] args, int count)
        {
            if (args.Length < count)
            {
                throw new ArgumentException(
                    $"Expected at least {count} arguments, but received {args.Length}",
                    nameof(args));
            }
        }

        public static T UnpackArgs<T>(object[] args)
        {
            if (args.Length != 1)
            {
                throw new ArgumentException(
                    $"Expected 1 arg, but found {args.Length}",
                    nameof(args));
            }

            return Unpack<T>(args[0]);
        }

        public static (T, U) UnpackArgs<T, U>(object[] args)
        {
            if (args.Length != 2)
            {
                throw new ArgumentException(
                    $"Expected 2 args, but found {args.Length}",
                    nameof(args));
            }

            return (Unpack<T>(args[0]), Unpack<U>(args[1]));
        }

        public static (T, U, V) UnpackArgs<T, U, V>(object[] args)
        {
            if (args.Length != 3)
            {
                throw new ArgumentException(
                    $"Expected 3 args, but found {args.Length}",
                    nameof(args));
            }

            return (Unpack<T>(args[0]), Unpack<U>(args[1]), Unpack<V>(args[2]));
        }

        public static T Unpack<T>(object arg)
        {
            if (arg is null)
            {
                return default;
            }
            if (arg is T converted)
            {
                return converted;
            }
            throw new ArgumentException($"Expected {typeof(T)}, but found {arg?.GetType()}");
        }
    }
}
