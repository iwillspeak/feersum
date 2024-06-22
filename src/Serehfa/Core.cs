using System;
using System.Linq;

using Serehfa.Attributes;

namespace Serehfa;

using static ArgHelpers;

[LispLibrary("feersum", "serehfa", "core")]
public static class Core
{
    [LispExport("procedure?")]
    public static object IsProcedure(object[] args)
    {
        var proc = UnpackArgs<object>(args);
        return proc is Func<object[], object>;
    }

    [LispExport("core-apply-vec")]
    public static object CoreApply(object[] args)
    {
        var (target, targetArgs) = UnpackArgs<Func<object[], object>, object[]>(args);
        return target(targetArgs);
    }
}
