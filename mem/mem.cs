using System;

public static class sg_wasm
{
    public static int __mem_size;
    public static IntPtr __mem;

    public static Span<byte> get_span(int addr, int len)
    {
        unsafe
        {
            return new Span<byte>((sg_wasm.__mem + (int) addr).ToPointer(), len);
        }
    }
}

