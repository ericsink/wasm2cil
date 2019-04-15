
#include <stdio.h>

void checkpoint(int n)
{
    fprintf(stderr, "checkpoint: %d\n", n);
}

void dumpf(int n, float f)
{
    fprintf(stderr, "dumpf: %d -- %3.4f\n", n, f);
}

void dumpb(int n, int b)
{
    fprintf(stderr, "dumpf: %d -- %02x\n", n, b);
}

int main()
{
    miniray();
}

