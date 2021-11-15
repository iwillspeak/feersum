using System;
using Serehfa.Attributes;
using System.Diagnostics;

namespace Serehfa
{
    using static ArgHelpers;

    [LispLibrary("scheme", "time")]
    public static class Time
    {
        [LispExport("current-second")]
        public static object CurrentSecond(object[] args)
        {
            CheckNoArgs(args);
            // FIXME: return fixnum
            return DateTimeOffset.UtcNow.ToUnixTimeMilliseconds() / 1000.0;
        }

        [LispExport("current-jiffy")]
        public static object CurrentJiffy(object[] args)
        {
            CheckNoArgs(args);
            // FIXME: return fixnum
            return (double)s_JiffyWatch.ElapsedTicks;
        }

        [LispExport("jiffies-per-second")]
        public static object JiffiesPerSeconds(object[] args)
        {
            CheckNoArgs(args);
            // FIXME: return fixnum
            return (double)Stopwatch.Frequency;
        }

        private static Stopwatch s_JiffyWatch = Stopwatch.StartNew();
    }
}
