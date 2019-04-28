using System;
using System.Runtime.InteropServices;
using System.IO;
using System.Text;
using System.Linq;
using System.Collections.Generic;

public static class __profile
{
    class FuncTotal
    {
        public int count { get; set; }
        public double time_all { get; set; }
        public double time_self { get; set; }
    }
    class Frame
    {
        public string name { get; private set; }
        public System.Diagnostics.Stopwatch timer_all { get; private set; }
        public System.Diagnostics.Stopwatch timer_self { get; private set; }
        public Frame(string s)
        {
            name = s;
            timer_all = new System.Diagnostics.Stopwatch();
            timer_self = new System.Diagnostics.Stopwatch();
        }
    }
    static Dictionary<string, FuncTotal> _funcs = new Dictionary<string, FuncTotal>();
    static Stack<Frame> _stack = new Stack<Frame>();

    static FuncTotal Find(string s)
    {
        if (!_funcs.TryGetValue(s, out var f))
        {
            f = new FuncTotal();
            _funcs.Add(s, f);
        }
        return f;
    }

    public static void Enter(string name)
    {
        var fn = Find(name);
        fn.count++;

        if (_stack.Count > 0)
        {
            _stack.Peek().timer_self.Stop();
        }

        var f = new Frame(name);
        f.timer_all.Start();
        f.timer_self.Start();

        _stack.Push(f);
    }
    public static void Exit(string name)
    {
        var f = _stack.Pop();
        f.timer_all.Stop();
        f.timer_self.Stop();

        var fn = Find(name);

        fn.time_all += f.timer_all.Elapsed.TotalMilliseconds;
        fn.time_self += f.timer_self.Elapsed.TotalMilliseconds;

        if (_stack.Count > 0)
        {
            _stack.Peek().timer_self.Start();
        }
    }
    public static void Report()
    {
        foreach (var ft in _funcs
            .OrderByDescending(kv => kv.Value.time_self)
            )
        {
            System.Console.WriteLine("{0} -- {1} -- {2} -- {3}", ft.Key, ft.Value.count, ft.Value.time_all, ft.Value.time_self);
        }
    }
}

