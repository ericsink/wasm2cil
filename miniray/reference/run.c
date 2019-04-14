
#include <stdio.h>

void checkpoint(int n)
{
    fprintf(stderr, "checkpoint: %d\n", n);
}

void dumpf(int n, float f)
{
    fprintf(stderr, "dumpf: %d -- %3.4f\n", n, f);
}

int main()
{
    miniray();
}

