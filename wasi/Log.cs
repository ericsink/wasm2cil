using System;
using System.Runtime.InteropServices;
using System.IO;
using System.Text;
using System.Linq;
using System.Collections.Generic;

public static class __log
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
}

