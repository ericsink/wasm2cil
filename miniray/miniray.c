
extern void checkpoint(int n);
extern void dumpf(int n, float f);

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

v R(v o) {

#if 1
  checkpoint(__LINE__);

  dumpf(__LINE__, o.x);
  dumpf(__LINE__, o.y);
  dumpf(__LINE__, o.z);

#endif

  f u=0;

  while (u<97) {
  }
  
  //checkpoint(__LINE__);
  return G(4,4,1);

}

int miniray() {

      //checkpoint(__LINE__);
      v p = R( G(-2,4,25));

  return 0;

}

