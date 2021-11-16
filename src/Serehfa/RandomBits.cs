using System;
using Serehfa.Attributes;
using static Serehfa.ArgHelpers;

namespace Serehfa
{
    [LispLibrary("feersum", "serehfa", "random-bits")]
    public static class RandomBits
    {
        [LispExport("random-integer")]
        public static object RandomInt(object[] args)
        {
            // FIXME: fixup for fixnums.
            var max = UnpackArgs<double>(args);
            lock (s_RandomInstance)
            {
                return (double)s_RandomInstance.Next((int)max);
            }
        }
    
        [LispExport("random-real")]
        public static object RandomDouble(object[] args)
        {
            CheckNoArgs(args);
            lock (s_RandomInstance)
            {
                return s_RandomInstance.NextDouble();
            }
        }

        private static Random s_RandomInstance = new Random();
    }
}
