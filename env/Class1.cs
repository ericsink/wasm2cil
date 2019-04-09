using System;

public static class env
{
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

