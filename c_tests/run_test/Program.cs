
public static class go
{
    public static int Main(string[] args)
    {
        int rc = 0;
        try
        {
            test.foo._start();
        }
        catch (ProcExitException e)
        {
            rc = e.ReturnCode;
        }
        return rc;
    }
}

