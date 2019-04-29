using System;
using System.Runtime.InteropServices;
using System.IO;
using System.Text;
using System.Linq;
using System.Collections.Generic;

public static class __log
{
    public static void Enter(string s, object[] parms)
    {
        System.Console.WriteLine("entering {0}", s);
        foreach (var p in parms)
        {
            System.Console.WriteLine("    {0}", p.ToString());
        }
    }

    public static void Exit(string s, object v)
    {
        System.Console.WriteLine("exiting {0}: {1}", s, v.ToString());
    }

    public static void Exit(string s)
    {
        System.Console.WriteLine("exiting {0}", s);
    }

#if not
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
#endif
}

