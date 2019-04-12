using System;

public static class env
{
    public static int __stack_pointer;
    public static int __extenddftf2(int n, double f)
    {
        throw new NotImplementedException();
    }
    public static int __subtf3(int a, long b, long c, long d, long e)
    {
        throw new NotImplementedException();
    }
    public static int __addtf3(int a, long b, long c, long d, long e)
    {
        throw new NotImplementedException();
    }
    public static int __trunctfdf2(long a, long b)
    {
        throw new NotImplementedException();
    }
    public static int __gttf2(long a,long b, long c, long d)
    {
        throw new NotImplementedException();
    }
    public static int __multf3(int a, long b, long c, long d, long e)
    {
        throw new NotImplementedException();
    }
    public static int __getf2(long a, long b, long c, long d)
    {
        throw new NotImplementedException();
    }
    public static int __divtf3(int a, long b, long c, long d, long e)
    {
        throw new NotImplementedException();
    }
    public static int __lttf2(long a,long b, long c, long d)
    {
        throw new NotImplementedException();
    }
    public static int __fixtfsi(long a, long b)
    {
        throw new NotImplementedException();
    }
    public static int __floatsitf(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static int __floatditf(int a, long b)
    {
        throw new NotImplementedException();
    }
    public static int sqlite3_os_init()
    {
        throw new NotImplementedException();
    }
    public static int sqlite3_os_end()
    {
        throw new NotImplementedException();
    }
    public static int strcmp(int a, int c)
    {
        throw new NotImplementedException();
    }
    public static int strlen(int a)
    {
        throw new NotImplementedException();
    }
    public static int memset(int p, int c, int sz)
    {
        throw new NotImplementedException();
    }
    public static int localtime(int a)
    {
        throw new NotImplementedException();
    }
    public static int memcpy(int dest, int src, int n)
    {
        throw new NotImplementedException();
    }
    public static int memmove(int dest, int src, int n)
    {
        throw new NotImplementedException();
    }
    public static int memcmp(int a, int b, int c)
    {
        throw new NotImplementedException();
    }
    public static int strncmp(int a, int b, int c)
    {
        throw new NotImplementedException();
    }
    public static int strcspn(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static int strrchr(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static int malloc(int a)
    {
        throw new NotImplementedException();
    }
    public static int free(int a)
    {
        throw new NotImplementedException();
    }
    public static int realloc(int a, int b)
    {
        throw new NotImplementedException();
    }

    public static IntPtr __mem_only_imported_in_one_test;

    public static IntPtr __my_mem;

    public static IntPtr __linear_memory;

    public static int puts(int x)
    {
        // TODO use x as the offset within mem, grab until a zero,
        // marshal.copy, convert to string, and write to console.
        System.Console.WriteLine("{0}", x);
        return 0;
    }

    public static byte[] GetResource(System.Reflection.Assembly a, string name)
    {
        using (var strm = a.GetManifestResourceStream(name))
        {
            var ms = new System.IO.MemoryStream();
            strm.CopyTo(ms);
            return ms.ToArray();
        }
    }
}

