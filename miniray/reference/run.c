
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

int main()
{
    miniray();
}

