using System;
using System.Runtime.InteropServices;

public static class wasi_unstable
{
    public static int __mem_size;
    public static IntPtr __mem;

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
    public static int clock_time_get(int clock_id, long precision, int addr_result)
    {
        switch (clock_id)
        {
            case 0:
                TimeSpan t = DateTime.UtcNow - new DateTime(1970, 1, 1);
                var ms = (long) (t.TotalMilliseconds);
                var ns = ms * 1000 * 1000;
                var ia = new long[] { ns };
                Marshal.Copy(ia, 0, __mem + addr_result, 1);
                return 0;
            default: throw new NotImplementedException();
        }
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
    static System.IO.Stream _stderr;
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
            case 2:
                if (_stderr == null)
                {
                    _stderr = System.Console.OpenStandardError();
                }
                strm = _stderr;
                break;
            default:
                throw new NotImplementedException();
        }

        var a_iovecs = new int[num_iovecs * 2];
        Marshal.Copy(__mem + p_a_iovecs, a_iovecs, 0, num_iovecs * 2);

        int total_len = 0;
        for (int i=0; i<num_iovecs; i++)
        {
            var addr = a_iovecs[i * 2];
            var len = a_iovecs[i * 2 + 1];
            var ba = new byte[len];
            Marshal.Copy(__mem + addr, ba, 0, len);
            strm.Write(ba, 0, len);
            total_len += len;
        }

        var ia = new int[] { total_len };
        Marshal.Copy(ia, 0, __mem + p_written, 1);

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
    public static int main(int a, int b)
    {
        // TODO this shouldn't be here
        throw new NotImplementedException();
    }
    public static int getcwd(int a, int b)
    {
        throw new NotImplementedException();
    }
    public static int localtime(int n)
    {
        // TODO why isn't this found in wasi sysroot?
        throw new NotImplementedException();
    }

}

