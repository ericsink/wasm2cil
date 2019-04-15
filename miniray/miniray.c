
extern void checkpoint(int n);
extern void dumpf(int n, float f);
extern void dumpb(int n, unsigned char b);

typedef float f;
typedef struct { f x,y,z; } v;

v G(f x, f y, f z) { 
    v n={x,y,z}; 
#if 0
  dumpf(__LINE__, n.x);
  dumpf(__LINE__, n.y);
  dumpf(__LINE__, n.z);
#endif
    return n; 
    }

void R(v o) {

#if 1
  {
    unsigned char* p = (unsigned char*) &o;
    dumpb(__LINE__, p[0]);
    dumpb(__LINE__, p[1]);
    dumpb(__LINE__, p[2]);
    dumpb(__LINE__, p[3]);
    dumpb(__LINE__, p[4]);
    dumpb(__LINE__, p[5]);
    dumpb(__LINE__, p[6]);
    dumpb(__LINE__, p[7]);
  }
  dumpf(__LINE__, o.x);
  dumpf(__LINE__, o.y);
  dumpf(__LINE__, o.z);
#endif

  f u=0;

  while (u<97) {
  }

}

int miniray() {

      //checkpoint(__LINE__);
      R( G(-2,4,25));

  return 0;

}

