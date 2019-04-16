
public static class go
{
    public static int Main(string[] args)
    {
        var x = sqlite3.foo.sqlite3_libversion_number();
        System.Console.WriteLine("{0}", x);
        var q = sqlite3.foo.test2();
        System.Console.WriteLine("{0}", q);
        return 0;
    }
}

