using System;
using System.Linq;
using Serehfa.Attributes;
using static Serehfa.ArgHelpers;

namespace Serehfa
{
    [LispLibrary("scheme", "process-context")]
    public static class ProcessContext
    {
        [LispExport("command-line")]
        public static object GetCommandLine(object[] args)
        {
            CheckNoArgs(args);
            return Lists.ListNew(
                Environment.GetCommandLineArgs()
                    .Cast<object>()
                    .ToArray());
        }

        [LispExport("exit")]
        public static object Exit(object[] args)
        {
            Environment.Exit(ExitStatusFromArgs(args));
            return null;
        }

        [LispExport("emergency-exit")]
        public static object EmergencyExit(object[] args)
        {
            Environment.ExitCode = ExitStatusFromArgs(args);
            Environment.FailFast("Scheme emergency exit");
            return null;
        }

        [LispExport("get-environment-variable")]
        public static object GetEnvironmentVariable(object[] args)
        {
            var name = UnpackArgs<string>(args);
            return Environment.GetEnvironmentVariable(name);
        }

        [LispExport("get-environment-variables")]
        public static object GetEnvironmentVariables(object[] args)
        {
            CheckNoArgs(args);
            var envVars = Environment.GetEnvironmentVariables();
            var envVarsAlist = new object[envVars.Count];
            int i = 0;
            foreach (System.Collections.DictionaryEntry envVar in envVars)
            {
                envVarsAlist[i++] = new ConsPair(envVar.Key.ToString(), envVar.Value.ToString());
            }
            return Lists.ListNew(envVarsAlist);
        }

        private static int ExitStatusFromArgs(object[] args)
        {
            if (args.Length == 0)
            {
                return 0;
            }

            return UnpackArgs<object>(args) switch
            {
                true => 0,
                false => 255,
                double d => (int)d,
                int i => i,
                object other => other.GetHashCode(),
            };
        }
    }
}
