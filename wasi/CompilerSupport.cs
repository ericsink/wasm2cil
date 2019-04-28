using System;
using System.Runtime.InteropServices;
using System.IO;
using System.Text;
using System.Linq;
using System.Collections.Generic;

public static class __compiler_support
{
    public static long popcnt_i64(long x)
    {
		x -= (x >> 1) & 0x5555555555555555L;
		x = (x & 0x3333333333333333L) + ((x >> 2) & 0x3333333333333333L);
		x = (x + (x >> 4)) & 0x0f0f0f0f0f0f0f0fL;
		return (x * 0x0101010101010101L) >> 56;
    }
    public static int popcnt_i32(int x)
    {
		x -= x >> 1 & 0x55555555;
		x = (x >> 2 & 0x33333333) + (x & 0x33333333);
		x = (x >> 4) + x & 0x0f0f0f0f;
		x += x >> 8;
		x += x >> 16;
		return x & 0x0000003f;
    }
    public static long clz_i64(long x)
    {
		const long numIntBits = sizeof(long) * 8; //compile time constant
		//do the smearing
		x |= x >> 1; 
		x |= x >> 2;
		x |= x >> 4;
		x |= x >> 8;
		x |= x >> 16;
		x |= x >> 32;
        // TODO inline popcnt here?
		return numIntBits - popcnt_i64(x);
    }
    public static int clz_i32(int x)
    {
		// https://stackoverflow.com/questions/10439242/count-leading-zeroes-in-an-int32
		const int numIntBits = sizeof(int) * 8; //compile time constant
		//do the smearing
		x |= x >> 1; 
		x |= x >> 2;
		x |= x >> 4;
		x |= x >> 8;
		x |= x >> 16;
		//count the ones
		x -= x >> 1 & 0x55555555;
		x = (x >> 2 & 0x33333333) + (x & 0x33333333);
		x = (x >> 4) + x & 0x0f0f0f0f;
		x += x >> 8;
		x += x >> 16;
		return numIntBits - (x & 0x0000003f); //subtract # of 1s from 32
    }
    public static long ctz_i64(long i)
    {
        if (i == 0) return 64L;
        int count = 0;
        while ((i & 0x01L) == 0)
        {
            i = i >> 1;
            count++;
        }
        return count;
    }
    public static int ctz_i32(int i)
    {
        if (i == 0) return 32;
        int count = 0;
        while ((i & 0x01L) == 0)
        {
            i = i >> 1;
            count++;
        }
        return count;
    }
}

