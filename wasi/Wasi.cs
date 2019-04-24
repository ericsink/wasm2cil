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

public static partial class wasi_unstable
{
    [StructLayout(LayoutKind.Explicit, Size=24)]
    struct __wasi_fdstat_t 
    {
        [FieldOffset(0)] public byte fs_filetype;
        [FieldOffset(2)] public ushort fs_flags;
        [FieldOffset(8)] public ulong fs_rights_base;
        [FieldOffset(16)] public ulong fs_rights_inheriting;
    }

    [StructLayout(LayoutKind.Explicit, Size=8)]
    struct __wasi_prestat_t 
    {
        [FieldOffset(0)] public byte pr_type;
        [FieldOffset(4)] public int pr_name_len;
    }

    [StructLayout(LayoutKind.Explicit, Size=56)]
    struct __wasi_filestat_t 
    {
        [FieldOffset(0)] public ulong st_dev;
        [FieldOffset(8)] public ulong st_ino;
        [FieldOffset(16)] public byte st_filetype;
        [FieldOffset(20)] public uint st_nlink;
        [FieldOffset(24)] public ulong st_size;
        [FieldOffset(32)] public ulong st_atim;
        [FieldOffset(40)] public ulong st_mtim;
        [FieldOffset(48)] public ulong st_ctim;
    }

    [StructLayout(LayoutKind.Explicit, Size=8)]
    struct __wasi_ciovec_t 
    {
        [FieldOffset(0)] public uint buf;
        [FieldOffset(4)] public uint buf_len;
    }

    [StructLayout(LayoutKind.Explicit, Size=8)]
    struct __wasi_iovec_t 
    {
        [FieldOffset(0)] public uint buf;
        [FieldOffset(4)] public uint buf_len;
    }

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
                    return null;
                }
        }

    }

    // TODO fds are supposed to be more random
    static int _nextFd = 10;
    static int get_new_fd()
    {
        return _nextFd++;
    }

    public static int path_open(
		int dirfd,
		int dirflags,
		int addr_path,
		int path_len,
		int oflags,
		long fs_rights_base,
		long fs_rights_inheriting,
		int fs_flags,
		int addr_fd
		)
    {
        // TODO very simplistic implementation
        //System.Console.WriteLine("dirfd: {0}", dirfd);
        var path = util.from_utf8(__mem + addr_path, path_len);
        //System.Console.WriteLine("path: {0}", path);
        var fi = new FileInfo(path);
        FileMode fm;
        {
            var creat = (oflags & __WASI_O_CREAT) != 0;
            var excl = (oflags & __WASI_O_EXCL) != 0;
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
            var fd = get_new_fd();
            _files[fd] = pair;
            Marshal.WriteInt32(__mem + addr_fd, fd);
            return __WASI_ESUCCESS;
        }
        catch (FileNotFoundException)
        {
            return __WASI_ENOENT;
        }
    }

    public static int fd_prestat_dir_name(int fd, int addr_path, int len)
    {
        switch (fd)
        {
            case 3:
                Marshal.WriteByte(__mem + addr_path, 46); // 46 is ascii for .
                //Marshal.WriteByte(__mem + addr_path + 1, 0); // assuming no need for z terminator
                return __WASI_ESUCCESS;
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
                __wasi_prestat_t st;
                st.pr_type = __WASI_PREOPENTYPE_DIR;
                st.pr_name_len = 1; // assuming no need for z terminator
                Marshal.StructureToPtr(st, __mem + addr, false);
                return __WASI_ESUCCESS;
            default:
                return __WASI_EBADF;
        }
    }
    static byte[][] __environ;
    public static void set_environ(Dictionary<string,string> d)
    {
        __environ = new byte[d.Count][];
        int i = 0;
        foreach (var kv in d)
        {
            var s = $"{kv.Key}={kv.Value}";
            __environ[i] = util.to_utf8_z(s);
            i++;
        }
    }
    public static int environ_sizes_get(int addr_environ_count, int addr_environ_buf_size)
    {
        if (__environ != null)
        {
            Marshal.WriteInt32(__mem + addr_environ_count, __environ.Length);
            int len = 0;
            foreach (var ba in __environ)
            {
                len += ba.Length;
            }
            Marshal.WriteInt32(__mem + addr_environ_buf_size, len);
        }
        else
        {
            Marshal.WriteInt32(__mem + addr_environ_count, 0);
            Marshal.WriteInt32(__mem + addr_environ_buf_size, 0);
        }
        return __WASI_ESUCCESS;
    }
    public static int environ_get(int addr_argv, int addr_argv_buf)
    {
        int sofar = 0;
        for (int i=0; i<__environ.Length; i++)
        {
            Marshal.WriteInt32(__mem + addr_argv + i * 4, addr_argv_buf + sofar);
            var ba = __environ[i];
            Marshal.Copy(ba, 0, __mem + addr_argv_buf + sofar, ba.Length);
            sofar += ba.Length;
        }
        return __WASI_ESUCCESS;
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
        return __WASI_ESUCCESS;
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
        return __WASI_ESUCCESS;
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
            return __WASI_ESUCCESS;
        }
        else
        {
            return __WASI_EBADF;
        }
    }
    public static int clock_time_get(int clock_id, long precision, int addr_result)
    {
        switch ((uint) clock_id)
        {
            case __WASI_CLOCK_REALTIME:
                TimeSpan t = DateTime.UtcNow - new DateTime(1970, 1, 1);
                var ms = (long) (t.TotalMilliseconds);
                var ns = ms * 1000 * 1000;
                Marshal.WriteInt64(__mem + addr_result, ns);
                return __WASI_ESUCCESS;
            case __WASI_CLOCK_MONOTONIC:
                throw new NotImplementedException();
            case __WASI_CLOCK_PROCESS_CPUTIME_ID:
                throw new NotImplementedException();
            case __WASI_CLOCK_THREAD_CPUTIME_ID:
                throw new NotImplementedException();
            default: 
                throw new NotImplementedException();
        }
    }
    public static int fd_close(int fd)
    {
        //System.Console.WriteLine("fd_close: {0}", fd);
        if (_files.TryGetValue(fd, out var pair))
        {
            pair.Stream.Close();
            _files.Remove(fd);
            return __WASI_ESUCCESS;
        }
        else
        {
            return __WASI_EBADF;
        }
    }
    public static int fd_sync(int fd)
    {
        //System.Console.WriteLine("fd_sync: fd {0}", fd);
        // TODO
        return __WASI_ESUCCESS;
    }
    public static int fd_seek(int fd, long offset, int whence, int addr_newoffset)
    {
        //System.Console.WriteLine("fd_seek: fd {0} offset {1} whence {2}", fd, offset, whence);
        FilePair fp;
        if (!_files.TryGetValue(fd, out fp))
        {
            return __WASI_EBADF;
        }
        SeekOrigin origin;
        long relpos;
        switch (whence)
        {
            case 0: // cur
                origin = SeekOrigin.Current;
                relpos = fp.Stream.Position;
                break;
            case 1: // end
                origin = SeekOrigin.End;
                fp.Info.Refresh();
                relpos = fp.Info.Length;
                break;
            case 2: // set
                origin = SeekOrigin.Begin;
                relpos = 0;
                break;
            default:
                throw new NotImplementedException();
        }
        if ((relpos + offset) < 0)
        {
            return __WASI_EINVAL;
        }
        var newpos = fp.Stream.Seek(offset, origin);
        write_u64(addr_newoffset, (ulong) newpos);
        return __WASI_ESUCCESS;
    }
    public static int fd_read(int fd, int addr_iovecs, int iovecs_len, int addr_nread)
    {
        //System.Console.WriteLine("fd_read: {0}  addr_iovecs: {1}  iovecs_len: {2}  addr_nread: {3}", fd, addr_iovecs, iovecs_len, addr_nread);
        Span<__wasi_iovec_t> a_iovecs;
        unsafe
        {
            a_iovecs = new Span<__wasi_iovec_t>((__mem + addr_iovecs).ToPointer(), iovecs_len);
        }

        var strm = get_stream_for_fd(fd);
        if (strm == null)
        {
            return __WASI_EBADF;
        }

        int total_len = 0;
        for (int i=0; i<iovecs_len; i++)
        {
            var addr = a_iovecs[i].buf;
            var len = a_iovecs[i].buf_len;
            //System.Console.WriteLine("    addr: {0}  len: {1}", addr, len);
            Span<byte> dest;
            unsafe
            {
                dest = new Span<byte>((__mem + (int) addr).ToPointer(), (int) len);
            }
            var got = strm.Read(dest);
            //System.Console.WriteLine("    got: {0}  len: {1}", got, len);
            total_len += got;
        }

        //System.Console.WriteLine("    total_len: {0}", total_len);
        Marshal.WriteInt32(__mem + addr_nread, total_len);

        return __WASI_ESUCCESS;
    }
    public static int poll_oneoff(int a, int b, int c, int d)
    {
        throw new NotImplementedException();
    }
    public static int fd_write(int fd, int addr_iovecs, int iovecs_len, int addr_nwritten)
    {
        //System.Console.WriteLine("fd_write: {0} {1} {2} {3}", fd, addr_iovecs, iovecs_len, addr_nwritten);
        Span<__wasi_ciovec_t> a_iovecs;
        unsafe
        {
            a_iovecs = new Span<__wasi_ciovec_t>((__mem + addr_iovecs).ToPointer(), iovecs_len);
        }

        var strm = get_stream_for_fd(fd);
        if (strm == null)
        {
            return __WASI_EBADF;
        }

        int total_len = 0;
        for (int i=0; i<iovecs_len; i++)
        {
            var addr = a_iovecs[i].buf;
            var len = a_iovecs[i].buf_len;
            //System.Console.WriteLine("    addr: {0}  len: {1}", addr, len);
            Span<byte> src;
            unsafe
            {
                src = new Span<byte>((__mem + (int) addr).ToPointer(), (int) len);
            }
            strm.Write(src);
            total_len += (int) len;
        }

        Marshal.WriteInt32(__mem + addr_nwritten, total_len);

        //System.Console.WriteLine("  done fd_write");

        return __WASI_ESUCCESS;
    }
    static void write_u64(int addr, ulong v)
    {
        var ba = BitConverter.GetBytes(v);
        Marshal.Copy(ba, 0, __mem + addr, ba.Length);
    }
    public static int fd_fdstat_get(int fd, int addr)
    {
        //System.Console.WriteLine("fd_fdstat_get: {0}", fd);
        switch (fd)
        {
            case 0: // stdin
            case 1: // stdout
            case 2: // stderr
                {
                    __wasi_fdstat_t st;
                    st.fs_filetype = __WASI_FILETYPE_CHARACTER_DEVICE;
					// TODO appropriate flags
                    st.fs_flags = 0; 
                    ulong rights = 0xffffffffffffffff;
                    rights = rights & (~__WASI_RIGHT_FD_SEEK);
                    rights = rights & (~__WASI_RIGHT_FD_TELL);
                    st.fs_rights_base = rights;
                    st.fs_rights_inheriting = 0; // TODO rights inherit
                    Marshal.StructureToPtr(st, __mem + addr, false);
                    return __WASI_ESUCCESS;
                }
            case 3:
                {
                    __wasi_fdstat_t st;
                    st.fs_filetype = __WASI_FILETYPE_DIRECTORY;
					// TODO appropriate flags
                    st.fs_flags = 0; 
                    ulong rights = 0xffffffffffffffff;
                    st.fs_rights_base = rights;
                    st.fs_rights_inheriting = rights; // TODO rights inherit
                    Marshal.StructureToPtr(st, __mem + addr, false);
                    return __WASI_ESUCCESS;
                }
            default:
                if (_files.TryGetValue(fd, out var strm))
                {
                    __wasi_fdstat_t st;
                    st.fs_filetype = __WASI_FILETYPE_REGULAR_FILE;
					// TODO appropriate flags
                    st.fs_flags = 0; 
                    ulong rights = 0xffffffffffffffff;
                    st.fs_rights_base = rights;
                    st.fs_rights_inheriting = rights; // TODO rights inherit
                    Marshal.StructureToPtr(st, __mem + addr, false);
                    return __WASI_ESUCCESS;
                }
                else
                {
                    return __WASI_EBADF;
                }
        }
    }
    public static int fd_fdstat_set_flags(int fd, int flags)
    {
        throw new NotImplementedException();
    }
    static void write_filestat(int addr, FileInfo fi)
    {
        __wasi_filestat_t st;
        st.st_dev = 0;
        st.st_ino = 0;
        st.st_filetype = __WASI_FILETYPE_REGULAR_FILE;
        st.st_nlink = 0;
        st.st_size = (ulong) fi.Length;
        st.st_atim = 0;
        st.st_mtim = 0;
        st.st_ctim = 0;
        Marshal.StructureToPtr(st, __mem + addr, false);
    }
    public static int path_filestat_get(
        int dirfd, 
        int flags, 
        int addr_path, 
        int path_len, 
        int addr_result
        )
    {
        var path = util.from_utf8(__mem + addr_path, path_len);
        //System.Console.WriteLine("path_filestat_get: {0}", path);
        var fi = new FileInfo(path);
        if (!fi.Exists)
        {
            return __WASI_ENOENT;
        }
        write_filestat(addr_result, fi);
        return __WASI_ESUCCESS;
    }
    public static int path_rename(
        int old_fd, 
        int addr_old_path, 
        int old_path_len, 
        int new_fd, 
        int addr_new_path, 
        int new_path_len
        )
    {
        var old_path = util.from_utf8(__mem + addr_old_path, old_path_len);
        var new_path = util.from_utf8(__mem + addr_new_path, new_path_len);
        throw new NotImplementedException();
    }
    public static int path_unlink_file(
        int dirfd, 
        int addr_path, 
        int path_len
        )
    {
        var path = util.from_utf8(__mem + addr_path, path_len);
        //System.Console.WriteLine("path_unlink_file: {0}", path);
        File.Delete(path);
        return __WASI_ESUCCESS;
    }
    public static int path_remove_directory(
        int fd, 
        int addr_path, 
        int path_len
        )
    {
        var path = util.from_utf8(__mem + addr_path, path_len);
        try
        {
            Directory.Delete(path);
            return __WASI_ESUCCESS;
        }
        catch (IOException)
        {
            // TODO check to see if this is the right error code.
            // this exception can be thrown for other reasons.
            return __WASI_ENOTEMPTY;
        }
    }
    public static int path_link(int a, int b, int c, int d, int e, int f, int g)
    {
        throw new NotImplementedException();
    }
    public static int path_create_directory(
        int fd, 
        int addr_path, 
        int path_len
        )
    {
        var path = util.from_utf8(__mem + addr_path, path_len);
        throw new NotImplementedException();
    }
    public static int fd_readdir(
        int fd, 
        int addr_buf, 
        int buf_len, 
        long cookie, 
        int addr_bufused
        )
    {
        throw new NotImplementedException();
    }
    public static int path_readlink(
        int fd, 
        int addr_path, 
        int path_len, 
        int addr_buf, 
        int buf_len, 
        int addr_bufused
        )
    {
        throw new NotImplementedException();
    }
    public static int path_symlink(
        int addr_old_path, 
        int old_path_len, 
        int fd, 
        int addr_new_path, 
        int new_path_len
        )
    {
        throw new NotImplementedException();
    }
}

public static class env
{
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

