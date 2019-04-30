
using System;
using System.Linq;

public static class go
{
    public static int Main(string[] args)
    {
        var newargs = new string[] {"sqlite"}.Concat(args).ToArray();
        wasi_unstable.set_args(newargs);

        int rc = 0;
        try
        {
            sqlite3.foo._start();
        }
        catch (ProcExitException e)
        {
            rc = e.ReturnCode;
        }
        __profile.Report();
        return rc;
    }
}

