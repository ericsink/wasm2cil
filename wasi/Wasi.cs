using System;
using System.Runtime.InteropServices;
using System.IO;
using System.Text;
using System.Collections.Generic;

public class ProcExitException : Exception
{
    public int ReturnCode { get; private set; }
    public ProcExitException(int rc)
    {
        ReturnCode = rc;
    }
}

public static class util
{
    public static byte[] to_utf8(string sourceText)
    {
        if (sourceText == null)
        {
            return null;
        }

        int nlen = Encoding.UTF8.GetByteCount(sourceText);

        var byteArray = new byte[nlen];
        nlen = Encoding.UTF8.GetBytes(sourceText, 0, sourceText.Length, byteArray, 0);

        return byteArray;
    }

    public static byte[] to_utf8_z(string sourceText)
    {
        if (sourceText == null)
        {
            return null;
        }

        int nlen = Encoding.UTF8.GetByteCount(sourceText) + 1;

        var byteArray = new byte[nlen];
        byteArray = new byte[nlen];
        nlen = Encoding.UTF8.GetBytes(sourceText, 0, sourceText.Length, byteArray, 0);
        byteArray[nlen] = 0;

        return byteArray;
    }

#if not
    private static int GetNativeUTF8Size(System.IntPtr nativeString)
    {
        var offset = 0;

        if (nativeString != IntPtr.Zero)
        {
            while (Marshal.ReadByte(nativeString, offset) > 0)
            {
                offset++;
            }

            offset++;
        }

        return offset;
    }

    public static string from_utf8(IntPtr nativeString)
    {
        string result = null;

        if (nativeString != IntPtr.Zero)
        {
            int size = GetNativeUTF8Size(nativeString);
            var array = new byte[size - 1];
            Marshal.Copy(nativeString, array, 0, size - 1);
            result = Encoding.UTF8.GetString(array, 0, array.Length);
        }

        return result;
    }
#endif

    public static string from_utf8(IntPtr nativeString, int size)
    {
        string result = null;

        if (nativeString != IntPtr.Zero)
        {
            var array = new byte[size];
            Marshal.Copy(nativeString, array, 0, size);
            result = Encoding.UTF8.GetString(array, 0, array.Length);
        }

        return result;
    }
}

public static class wasi_unstable
{
    public static int __mem_size;
    public static IntPtr __mem;

    class FilePair
    {
        public FileInfo Info { get; set; }
        public FileStream Stream { get; set; }
    }

    static Dictionary<int,FilePair> _files = new Dictionary<int,FilePair>();

    static System.IO.Stream _stdin;
    static System.IO.Stream _stdout;
    static System.IO.Stream _stderr;
    static Stream get_stream_for_fd(int fd)
    {
        switch (fd)
        {
            case 0:
                if (_stdin == null)
                {
                    _stdin = System.Console.OpenStandardInput();
                }
                return _stdin;
            case 1:
                if (_stdout == null)
                {
                    _stdout = System.Console.OpenStandardOutput();
                }
                return _stdout;
            case 2:
                if (_stderr == null)
                {
                    _stderr = System.Console.OpenStandardError();
                }
                return _stderr;
            default:
                if (_files.TryGetValue(fd, out var fp))
                {
                    return fp.Stream;
                }
                else
                {
                    // TODO probably an error code
                    throw new NotImplementedException();
                }
        }

    }

    // TODO fds are supposed to be more random
    static int _nextFd = 10;

    public static int path_open(
		int dirfd,
		int dirflags,
		int addr_path,
		int len_path,
		int oflags,
		long fs_rights_base,
		long fs_rights_inheriting,
		int fs_flags,
		int addr_fd
		)
    {
        // TODO very simplistic implementation
        //System.Console.WriteLine("dirfd: {0}", dirfd);
        var path = util.from_utf8(__mem + addr_path, len_path);
        //System.Console.WriteLine("path: {0}", path);
        var fi = new FileInfo(path);
        FileMode fm;
        {
            var creat = (oflags & 0x01) != 0;
            var excl = (oflags & 0x04) != 0;
            if (creat)
            {
                if (excl)
                {
                    fm = FileMode.CreateNew;
                }
                else
                {
                    fm = FileMode.OpenOrCreate;
                }
            }
            else
            {
                fm = FileMode.Open;
            }
        }
        try
        {
            var strm = fi.Open(fm);
            var pair = new FilePair { Info = fi, Stream = strm };
            var fd = _nextFd++;
            _files[fd] = pair;
            Marshal.WriteInt32(__mem + addr_fd, fd);
            return 0;
        }
        catch (FileNotFoundException)
        {
            return 44; // ENOENT
        }
    }

    public static int fd_prestat_dir_name(int fd, int addr_path, int len)
    {
        switch (fd)
        {
            case 3:
                Marshal.WriteByte(__mem + addr_path, 46); // .
                //Marshal.WriteByte(__mem + addr_path + 1, 0);
                return 0;
            default:
                throw new NotImplementedException(string.Format("fd {0}  len {1}", fd, len));
        }
    }
    public static int fd_prestat_get(int fd, int addr)
    {
        //System.Console.WriteLine("fd_prestat_get: {0}", fd);
        // there is a loop that tries preopened file descriptors
        // starting at 3 until it finds a bad one.
        switch (fd)
        {
            case 3:
                Marshal.WriteByte(__mem + addr, 0); // preopentype_dir
                Marshal.WriteInt32(__mem + addr + 4, 1);
                return 0;
            default:
                return 8; // EBADF
        }
    }
    public static int environ_sizes_get(int addr_environ_count, int addr_environ_buf_size)
    {
        // TODO for now, no environment variables
        Marshal.WriteInt32(__mem + addr_environ_count, 0);
        Marshal.WriteInt32(__mem + addr_environ_buf_size, 0);
        return 0;
    }
    public static int environ_get(int a, int b)
    {
        throw new NotImplementedException();
    }
    static byte[][] __args;
    public static void set_args(string[] a)
    {
        __args = new byte[a.Length][];
        for (int i=0; i<a.Length; i++)
        {
            __args[i] = util.to_utf8_z(a[i]);
        }
    }
    public static int args_sizes_get(int addr_argc, int addr_argv_buf_size)
    {
        if (__args != null)
        {
            Marshal.WriteInt32(__mem + addr_argc, __args.Length);
            int len = 0;
            foreach (var ba in __args)
            {
                len += ba.Length;
            }
            Marshal.WriteInt32(__mem + addr_argv_buf_size, len);
        }
        else
        {
            Marshal.WriteInt32(__mem + addr_argc, 0);
            Marshal.WriteInt32(__mem + addr_argv_buf_size, 0);
        }
        return 0;
    }
    public static int args_get(int addr_argv, int addr_argv_buf)
    {
        int sofar = 0;
        for (int i=0; i<__args.Length; i++)
        {
            Marshal.WriteInt32(__mem + addr_argv + i * 4, addr_argv_buf + sofar);
            var ba = __args[i];
            Marshal.Copy(ba, 0, __mem + addr_argv_buf + sofar, ba.Length);
            sofar += ba.Length;
        }
        return 0;
    }
    public static void proc_exit(int a)
    {
        throw new ProcExitException(a);
    }
    public static int fd_filestat_get(int fd, int addr_result)
    {
        if (_files.TryGetValue(fd, out var pair))
        {
            pair.Info.Refresh();
            write_filestat(addr_result, pair.Info);
            return 0;
        }
        else
        {
            // TODO err code
            throw new NotImplementedException();
        }
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
    public static int fd_close(int fd)
    {
        //System.Console.WriteLine("fd_close: {0}", fd);
        if (_files.TryGetValue(fd, out var pair))
        {
            pair.Stream.Close();
            _files.Remove(fd);
            return 0;
        }
        else
        {
            // TODO err code
            throw new NotImplementedException();
        }
    }
    public static int fd_sync(int fd)
    {
        //System.Console.WriteLine("fd_sync: fd {0}", fd);
        // TODO
        return 0;
    }
    public static int fd_seek(int fd, long offset, int whence, int addr_newoffset)
    {
        //System.Console.WriteLine("fd_seek: fd {0} offset {1} whence {2}", fd, offset, whence);
        var strm = get_stream_for_fd(fd);
        SeekOrigin origin;
        switch (whence)
        {
            case 0: // cur
                origin = SeekOrigin.Current;
                break;
            case 1: // end
                origin = SeekOrigin.End;
                break;
            case 2: // set
                origin = SeekOrigin.Begin;
                break;
            default:
                throw new NotImplementedException();
        }
        var newpos = strm.Seek(offset, origin);
        write_u64(addr_newoffset, (ulong) newpos);
        return 0;
    }
    public static int fd_read(int fd, int addr_iovecs, int iovecs_len, int addr_nread)
    {
        //System.Console.WriteLine("fd_read: {0}  addr_iovecs: {1}  iovecs_len: {2}  addr_nread: {3}", fd, addr_iovecs, iovecs_len, addr_nread);
        var a_iovecs = new int[iovecs_len * 2];
        Marshal.Copy(__mem + addr_iovecs, a_iovecs, 0, iovecs_len * 2);

        var strm = get_stream_for_fd(fd);

        int total_len = 0;
        for (int i=0; i<iovecs_len; i++)
        {
            var addr = a_iovecs[i * 2];
            var len = a_iovecs[i * 2 + 1];
            //System.Console.WriteLine("    addr: {0}  len: {1}", addr, len);
            var ba = new byte[len];
            // TODO ReadFully
            var got = strm.Read(ba, 0, len);
            //System.Console.WriteLine("    got: {0}  len: {1}", got, len);
            Marshal.Copy(ba, 0, __mem + addr, got);
            total_len += got;
        }

        //System.Console.WriteLine("    total_len: {0}", total_len);
        Marshal.WriteInt32(__mem + addr_nread, total_len);

        return 0;
    }
    public static int poll_oneoff(int a, int b, int c, int d)
    {
        throw new NotImplementedException();
    }
    public static int fd_write(int fd, int addr_iovecs, int iovecs_len, int addr_nwritten)
    {
        //System.Console.WriteLine("fd_write: {0} {1} {2} {3}", fd, addr_iovecs, iovecs_len, addr_nwritten);

        var a_iovecs = new int[iovecs_len * 2];
        Marshal.Copy(__mem + addr_iovecs, a_iovecs, 0, iovecs_len * 2);

        var strm = get_stream_for_fd(fd);

        int total_len = 0;
        for (int i=0; i<iovecs_len; i++)
        {
            var addr = a_iovecs[i * 2];
            var len = a_iovecs[i * 2 + 1];
            var ba = new byte[len];
            Marshal.Copy(__mem + addr, ba, 0, len);
            // TODO WriteFully
            strm.Write(ba, 0, len);
            total_len += len;
        }

        Marshal.WriteInt32(__mem + addr_nwritten, total_len);

        //System.Console.WriteLine("  done fd_write");

        return 0;
    }
    static void write_u64(int addr, ulong v)
    {
        var ba = BitConverter.GetBytes(v);
        Marshal.Copy(ba, 0, __mem + addr, ba.Length);
    }
    static void write_u32(int addr, uint v)
    {
        var ba = BitConverter.GetBytes(v);
        Marshal.Copy(ba, 0, __mem + addr, ba.Length);
    }
    static void add_all_rights(int addr)
    {
        // TODO temporary.  in each case, think about what rights
        // should actually be given.
        for (int i=0; i<8; i++)
        {
            Marshal.WriteByte(__mem + addr + i, 0xff);
        }
    }
    public static int fd_fdstat_get(int fd, int addr)
    {
        //System.Console.WriteLine("fd_fdstat_get: {0}", fd);
        switch (fd)
        {
            case 0: // stdin
                {
                    Marshal.WriteByte(__mem + addr + 0, 2); // character device
                    // TODO appropriate flags for stdin
                    Marshal.WriteInt16(__mem + addr + 2, 0); // flags
                    ulong rights = 0xffffffffffffffff;
                    rights = rights & (~0x04UL); // seek
                    rights = rights & (~0x20UL); // tell
                    write_u64(addr + 8, rights);
                    write_u64(addr + 16, 0); // TODO rights inherit
                    return 0;
                }
            case 1: // stdout
                {
                    Marshal.WriteByte(__mem + addr + 0, 2); // character device
                    // TODO appropriate flags for stdout
                    Marshal.WriteInt16(__mem + addr + 2, 0); // flags
                    ulong rights = 0xffffffffffffffff;
                    rights = rights & (~0x04UL); // seek
                    rights = rights & (~0x20UL); // tell
                    write_u64(addr + 8, rights);
                    write_u64(addr + 16, 0); // TODO rights inherit
                    return 0;
                }
            case 2: // stderr
                {
                    Marshal.WriteByte(__mem + addr + 0, 2); // character device
                    // TODO appropriate flags for stderr
                    Marshal.WriteInt16(__mem + addr + 2, 0); // flags
                    ulong rights = 0xffffffffffffffff;
                    rights = rights & (~0x04UL); // seek
                    rights = rights & (~0x20UL); // tell
                    write_u64(addr + 8, rights);
                    write_u64(addr + 16, 0); // TODO rights inherit
                    return 0;
                }
            case 3:
                {
                    Marshal.WriteByte(__mem + addr + 0, 3); // dir
                    // TODO appropriate flags for the pre dir
                    Marshal.WriteInt16(__mem + addr + 2, 0); // flags
                    add_all_rights(addr + 8); // TODO rights
                    add_all_rights(addr + 16); // TODO inherit
                    return 0;
                }
            default:
                if (_files.TryGetValue(fd, out var strm))
                {
                    Marshal.WriteByte(__mem + addr + 0, 4); // regular file
                    // TODO appropriate flags for this file
                    Marshal.WriteInt16(__mem + addr + 2, 0); // flags
                    add_all_rights(addr + 8); // TODO rights
                    add_all_rights(addr + 16); // TODO inherit
                    return 0;
                }
                else
                {
                    // TODO probably an error code
                    throw new NotImplementedException();
                }
        }
    }
    public static int fd_fdstat_set_flags(int a, int b)
    {
        throw new NotImplementedException();
    }
    static void write_filestat(int addr_result, FileInfo fi)
    {
        write_u64(addr_result + 0, 0); // device ID
        write_u64(addr_result + 8, 0); // inode
        Marshal.WriteByte(__mem + addr_result + 16, 4); // file type, 4, regular file
        write_u32(addr_result + 20, 0); // hard links
        write_u64(addr_result + 24, (ulong) (fi.Length)); // size
        write_u64(addr_result + 32, 0); // access timestamp
        write_u64(addr_result + 40, 0); // mod time
        write_u64(addr_result + 48, 0); // status change time
    }
    public static int path_filestat_get(
        int dirfd, 
        int flags, 
        int addr_path, 
        int len_path, 
        int addr_result
        )
    {
        var path = util.from_utf8(__mem + addr_path, len_path);
        //System.Console.WriteLine("path_filestat_get: {0}", path);
        var fi = new FileInfo(path);
        if (!fi.Exists)
        {
            return 44; // ENOENT
        }
        write_filestat(addr_result, fi);
        return 0;
    }
    public static int path_rename(int a, int b, int c, int d, int e, int f)
    {
        throw new NotImplementedException();
    }
    public static int path_unlink_file(
        int dirfd, 
        int addr_path, 
        int len_path
        )
    {
        var path = util.from_utf8(__mem + addr_path, len_path);
        //System.Console.WriteLine("path_unlink_file: {0}", path);
        File.Delete(path);
        return 0;
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
    // TODO this shouldn't be here.  but sqlite demo vfs needs it.
    public static int getcwd(int addr_buf, int len)
    {
        var cwd = Directory.GetCurrentDirectory();
        var full = Path.GetFullPath(Path.Combine(cwd, ".."));
        // TODO unixify
        full = ".";
        var ba = util.to_utf8_z(full);
        Marshal.Copy(ba, 0, wasi_unstable.__mem + addr_buf, ba.Length);
        return addr_buf;
    }

    public static void Trace(string s)
    {
        System.Console.WriteLine("{0}", s);
    }

    public static void Trace2(object v, string s)
    {
        if (v is null)
        {
            System.Console.WriteLine("{0} NULL", s);
        }
        else
        {
            System.Console.WriteLine("{0} : {1}", s, v);
        }
    }
    public static int clz_i64(long i)
    {
        // TODO this is such a dreadful hack
        return 64 - Convert.ToString(i, 2).Length;
    }
    public static int clz_i32(int i)
    {
        // TODO this is such a dreadful hack
        return 32 - Convert.ToString(i, 2).Length;
    }
    public static int ctz_i64(long i)
    {
        // TODO this is such a dreadful hack
        var s = Convert.ToString(i, 2);
        int count = 0;
        while (s[s.Length - 1 - count] == '0')
        {
            count++;
        }
        return count;
    }
    public static int ctz_i32(int i)
    {
        // TODO this is such a dreadful hack
        var s = Convert.ToString(i, 2);
        int count = 0;
        while (s[s.Length - 1 - count] == '0')
        {
            count++;
        }
        return count;
    }
}

