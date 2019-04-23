using System;
using System.Runtime.InteropServices;
using System.IO;
using System.Text;
using System.Collections.Generic;

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

