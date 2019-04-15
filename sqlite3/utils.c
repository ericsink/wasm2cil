
#include <stdlib.h>
#include <stdint.h>

size_t
strlen(const char* str)
{
  const char *s;

  for (s = str; *s; ++s);
  return(s - str);
}

int memcmp(const void *p1, const void *p2, size_t n)
{
    size_t i;

    /**
    * p1 and p2 are the same memory? easy peasy! bail out
    */
    if (p1 == p2)
    {
        return 0;
    }

    // This for loop does the comparing and pointer moving...
    for (i = 0; (i < n) && (*(uint8_t *)p1 == *(uint8_t *)p2);
        i++, p1 = 1 + (uint8_t *)p1, p2 = 1 + (uint8_t *)p2);

    //if i == length, then we have passed the test
    return (i == n) ? 0 : (*(uint8_t *)p1 - *(uint8_t *)p2);
}

void * __attribute__((weak)) memset(void * dest, int c, size_t n)
{
    unsigned char *s = dest;
    size_t k;

    /* Fill head and tail with minimal branching. Each
     * conditional ensures that all the subsequently used
     * offsets are well-defined and in the dest region. */

    if (!n) return dest;
    s[0] = s[n-1] = c;
    if (n <= 2) return dest;
    s[1] = s[n-2] = c;
    s[2] = s[n-3] = c;
    if (n <= 6) return dest;
    s[3] = s[n-4] = c;
    if (n <= 8) return dest;

    /* Advance pointer to align it at a 4-byte boundary,
     * and truncate n to a multiple of 4. The previous code
     * already took care of any head/tail that get cut off
     * by the alignment. */

    k = -(uintptr_t)s & 3;
    s += k;
    n -= k;
    n &= -4;
    n /= 4;

    uint32_t *ws = (uint32_t *)s;
    uint32_t wc = c & 0xFF;
    wc |= ((wc << 8) | (wc << 16) | (wc << 24));

    /* Pure C fallback with no aliasing violations. */
    for (; n; n--, ws++) *ws = wc;

    return dest;
}

