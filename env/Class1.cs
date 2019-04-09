using System;

public static class env
{
    public static IntPtr __mem_only_imported_in_one_test;

    public static IntPtr __my_mem;

    public static int puts(int x)
    {
        System.Console.WriteLine("{0}", x);
        return 0;
    }
}

