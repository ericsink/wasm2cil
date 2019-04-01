
using System;
using System.Collections.Generic;

class WasmStack
{
    Stack<object> _stk = new Stack<object>();

    public void push_i32(int i)
    {
        _stk.Push(i);
    }

    public void push_i32_bool(bool b)
    {
        _stk.Push(b ? 1 : 0);
    }

    public void push_i64(long i)
    {
        _stk.Push(i);
    }

    public void push_f32(float i)
    {
        _stk.Push(i);
    }

    public void push_f64(double i)
    {
        _stk.Push(i);
    }

    public int pop_i32()
    {
        var ob = _stk.Pop();
        return (int) ob;
    }

    public long pop_i64()
    {
        var ob = _stk.Pop();
        return (long) ob;
    }

    public int peek_i32()
    {
        var ob = _stk.Peek();
        return (int) ob;
    }

    public double pop_f64()
    {
        var ob = _stk.Pop();
        return (double) ob;
    }

    public float pop_f32()
    {
        var ob = _stk.Pop();
        return (float) ob;
    }

}

