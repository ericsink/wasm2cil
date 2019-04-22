
using System;
using System.Linq;

public static class go
{
    public static int Main(string[] args)
    {
        var x = sqlite3.foo.sqlite3_libversion_number();
        System.Console.WriteLine("{0}", x);

        var newargs = new string[] {"myapp"}.Concat(args).ToArray();
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
        return rc;
    }
}

