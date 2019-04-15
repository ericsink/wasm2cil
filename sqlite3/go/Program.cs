
public static class go
{
    public static int Main(string[] args)
    {
        var x = sqlite3.foo.sqlite3_libversion_number();
        System.Console.WriteLine("{0}", x);
        return 0;
    }
}

