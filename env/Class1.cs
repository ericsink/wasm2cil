using System;
using System.Runtime.InteropServices;

public static class wasi_unstable
{
    public static int path_open(
		int dirfd,
		int dirflags,
		int path,
		int path_len,
		int oflags,
		long fs_rights_base,
		long fs_rights_inheriting,
		int fs_flags,
		int fd
		)
    {
        throw new NotImplementedException();
    }

    public static int fd_prestat_dir_name(int a, int b, int c)
    {
        throw new NotImplementedException();
    }
    public static int fd_prestat_get(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static int environ_sizes_get(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static int environ_get(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static int args_sizes_get(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static int args_get(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static void proc_exit(int a)
    {
        throw new NotImplementedException();
    }
    public static int fd_filestat_get(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static int clock_time_get(int a, long b, int c)
    {
        throw new NotImplementedException();
    }
    public static int fd_close(int a)
    {
        throw new NotImplementedException();
    }
    public static int fd_sync(int a)
    {
        throw new NotImplementedException();
    }
    public static int fd_seek(int a, long b, int c, int d)
    {
        throw new NotImplementedException();
    }
    public static int fd_read(int a, int b, int c, int d)
    {
        throw new NotImplementedException();
    }
    public static int poll_oneoff(int a, int b, int c, int d)
    {
        throw new NotImplementedException();
    }
    static System.IO.Stream _stdout;
    public static int fd_write(int fd, int p_a_iovecs, int num_iovecs, int p_written)
    {
        System.IO.Stream strm;
        switch (fd)
        {
            case 1:
                if (_stdout == null)
                {
                    _stdout = System.Console.OpenStandardOutput();
                }
                strm = _stdout;
                break;
            default:
                throw new NotImplementedException();
        }

        var a_iovecs = new int[num_iovecs * 2];
        Marshal.Copy(env.__mem + p_a_iovecs, a_iovecs, 0, num_iovecs * 2);

        int total_len = 0;
        for (int i=0; i<num_iovecs; i++)
        {
            var addr = a_iovecs[i * 2];
            var len = a_iovecs[i * 2 + 1];
            var ba = new byte[len];
            Marshal.Copy(env.__mem + addr, ba, 0, len);
            strm.Write(ba, 0, len);
            total_len += len;
        }

        var ia = new int[] { total_len };
        Marshal.Copy(ia, 0, env.__mem + p_written, 1);

        return 0;
    }
    public static int fd_fdstat_get(int a, int b)
    {
        // TODO what is this supposed to do?
        return 0;
    }
    public static int path_filestat_get(int a, int b, int c, int d, int e)
    {
        throw new NotImplementedException();
    }
    public static int path_rename(int a, int b, int c, int d, int e, int f)
    {
        throw new NotImplementedException();
    }
    public static int path_unlink_file(int a, int b, int c)
    {
        throw new NotImplementedException();
    }
    public static int path_remove_directory(int a, int b, int c)
    {
        throw new NotImplementedException();
    }
    public static int path_link(int a, int b, int c, int d, int e, int f, int g)
    {
        throw new NotImplementedException();
    }
    public static int path_create_directory(int a, int b, int c)
    {
        throw new NotImplementedException();
    }
    public static int fd_readdir(int a, int b, int c, long d, int e)
    {
        throw new NotImplementedException();
    }
    public static int path_readlink(int a, int b, int c, int d, int e, int f)
    {
        throw new NotImplementedException();
    }
    public static int path_symlink(int a, int b, int c, int d, int e)
    {
        throw new NotImplementedException();
    }
}

public static class env
{
    public static int __stack_pointer;
    public static int main(int a, int b)
    {
        // TODO this shouldn't be here
        throw new NotImplementedException();
    }
    public static int getcwd(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static long get_ms()
    {
        TimeSpan t = DateTime.UtcNow - new DateTime(1970, 1, 1);
        return (long) (t.TotalMilliseconds);
    }
    public static void dump_i32(int n, int b)
    {
        System.Console.Error.WriteLine("dumpb: {0} -- {1}", n, b.ToString());
    }
    public static void dumpf(int n, float f)
    {
        System.Console.Error.WriteLine("dumpf: {0} -- {1}", n, f.ToString("0.0000"));
    }
    public static void checkpoint(int n)
    {
        System.Console.Error.WriteLine("checkpoint: {0}", n);
    }
    public static double exp2(double x)
    {
        return Math.Pow(2, x);
    }
    public static double pow(double x, double y)
    {
        //System.Console.Error.WriteLine("pow: {0}, {1}", x, y);
        return Math.Pow(x, y);
    }
    public static double atan2(double x, double y)
    {
        //System.Console.Error.WriteLine("atan2: {0}, {1}", x, y);
        return Math.Atan2(x, y);
    }
    public static double sqrt(double x)
    {
        System.Console.Error.WriteLine("sqrt: {0}", x);
        return Math.Sqrt(x);
    }
    public static double cos(double x)
    {
        //System.Console.Error.WriteLine("cos: {0}", x);
        return Math.Cos(x);
    }
    public static double sin(double x)
    {
        //System.Console.Error.WriteLine("sin: {0}", x);
        return Math.Sin(x);
    }
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

    public static int __mem_size;
    public static IntPtr __mem;

    public static IntPtr __mem_only_imported_in_one_test;

    public static IntPtr __my_mem;

    public static IntPtr __linear_memory;

    // TODO rm
    static System.IO.Stream _stdout;

    // TODO rm
    static void ensure()
    {
        if (_stdout == null)
        {
            _stdout = System.Console.OpenStandardOutput();
        }
    }

    // TODO rm
    public static int putchar(int i)
    {
        ensure();
        var ba = new byte[] { (byte) i };
        _stdout.Write(ba, 0, 1);
        return i;
    }

    // TODO rm
    public static int puts(int x)
    {
        // TODO use x as the offset within mem, grab until a zero,
        // marshal.copy, convert to string, and write to console.
        System.Console.WriteLine("{0}", x);
        return 0;
    }

}

