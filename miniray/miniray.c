
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
v S(v a, f s) { 
#if 0
  dumpf(__LINE__, a.x);
  dumpf(__LINE__, a.y);
  dumpf(__LINE__, a.z);
#endif
    v r = G(a.x*s, a.y*s, a.z*s); 
#if 0
  dumpf(__LINE__, r.x);
  dumpf(__LINE__, r.y);
  dumpf(__LINE__, r.z);
#endif
    return r;
    }

v R(v o) {

#if 1
  checkpoint(__LINE__);

  dumpf(__LINE__, o.x);
  dumpf(__LINE__, o.y);
  dumpf(__LINE__, o.z);

#endif

  f u=0, l=0, i=0, a=1;
  v p, n;

  while (u<97) {
  }
  
  //checkpoint(__LINE__);
  return G(4,4,1);

}

int miniray() {

      //checkpoint(__LINE__);
      v p = S( R( G(-2,4,25)), 255 );

  return 0;

}

