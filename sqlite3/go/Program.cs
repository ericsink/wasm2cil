
public static class go
{
    public static int Main(string[] args)
    {
        var x = sqlite3.foo.sqlite3_libversion_number();
        System.Console.WriteLine("{0}", x);

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

