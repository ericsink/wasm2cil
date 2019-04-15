
#include <stdio.h>
#include <sys/time.h>

long get_ms(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    long s = (long) (tv.tv_sec);
    long us = (long) (tv.tv_usec);
    long ms = us / 1000;
    long total = s * 1000 + ms;
    return total;
}

void checkpoint(int n)
{
    fprintf(stderr, "checkpoint: %d\n", n);
}

void dumpf(int n, float f)
{
    fprintf(stderr, "dumpf: %d -- %3.4f\n", n, f);
}

void dump_i32(int n, int v)
{
    fprintf(stderr, "dump_i32: %d -- %d\n", n, v);
}

int main()
{
    miniray();
}

