
using System.Collections.Generic;
using System.Linq;

public static class go
{
    public static int Main(string[] args)
    {
        var newargs = new string[] {"todo"}.Concat(args).ToArray();
        wasi_unstable.set_args(newargs);
        //wasi_unstable.set_environ(new Dictionary<string,string> { { "RUST_BACKTRACE", "1"} });

        int rc = 0;
        try
        {
            test.foo._start();
        }
        catch (ProcExitException e)
        {
            rc = e.ReturnCode;
        }
        __profile.Report(System.Console.Error);
        return rc;
    }
}

