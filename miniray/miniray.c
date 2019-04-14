//#include <stdio.h>
#define M_PI 3.1415926535897932384
//#include <math.h>

extern int putchar(int);
extern double sin(double);
extern double cos(double);
extern double sqrt(double);
extern double pow(double, double);
extern double atan2(double, double);

extern void checkpoint(int n);
extern void dumpf(int n, float f);

#define E return

typedef float f;
typedef struct { f x,y,z; } v;

f H=.5, Z=.33, Y=.66, I, x, y=-111;
v L, W={1,1,1}, F, P, C, M, N;

v G(f x, f y, f z) { 
    v n={x,y,z}; 
#if 0
  dumpf(__LINE__, n.x);
  dumpf(__LINE__, n.y);
  dumpf(__LINE__, n.z);
#endif
    return n; 
    }
v A(v a, v b, f c) { 
#if 0
  dumpf(__LINE__, a.x);
  dumpf(__LINE__, a.y);
  dumpf(__LINE__, a.z);

  dumpf(__LINE__, b.x);
  dumpf(__LINE__, b.y);
  dumpf(__LINE__, b.z);

  dumpf(__LINE__, c);
#endif
    v r = G(a.x+b.x*c, a.y+b.y*c, a.z+b.z*c); 
#if 0
  dumpf(__LINE__, r.x);
  dumpf(__LINE__, r.y);
  dumpf(__LINE__, r.z);
#endif
    return r;
    }
f O(v a, v b) { 
#if 0
  dumpf(__LINE__, a.x);
  dumpf(__LINE__, a.y);
  dumpf(__LINE__, a.z);
  dumpf(__LINE__, b.x);
  dumpf(__LINE__, b.y);
  dumpf(__LINE__, b.z);
#endif
    f r = a.x*b.x+a.y*b.y+a.z*b.z; 
#if 0
  dumpf(__LINE__, r);
#endif
    return r;
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
v _(v a) { 
#if 0
  dumpf(__LINE__, a.x);
  dumpf(__LINE__, a.y);
  dumpf(__LINE__, a.z);
#endif
  v r = S(a, 1/sqrt(O(a,a))); 
#if 0
  dumpf(__LINE__, r.x);
  dumpf(__LINE__, r.y);
  dumpf(__LINE__, r.z);
#endif
    return r;
    }

f U(f a) { return a<0?0:a>1?1:a; }

f Q(v c, v m) {
#if 0
  dumpf(__LINE__, c.x);
  dumpf(__LINE__, c.y);
  dumpf(__LINE__, c.z);
  dumpf(__LINE__, m.x);
  dumpf(__LINE__, m.y);
  dumpf(__LINE__, m.z);
#endif
  v D = A(P,c,-1); 
#if 0
  dumpf(__LINE__, D.x);
  dumpf(__LINE__, D.y);
  dumpf(__LINE__, D.z);
#endif
  f d = O(D,D);
  return d<I ? C=c,M=m,I=d:d;
}

f D(v p) {
#if 0
  dumpf(__LINE__, p.x);
  dumpf(__LINE__, p.y);
  dumpf(__LINE__, p.z);
#endif

  f x=0;

  // Encode the letters "mattz", see below for explanation:
  char* B = "BCJB@bJBHbJCE[FLL_A[FLMCA[CCTT`T", *b, o, a, y;

  I=99;

  P=p;

  // Interpret bytecodes by iterating over B in 2-byte chunks.
  for (b=B;*b;++b) {

    // Extract o, a, x, y from the current two bytes:
    //
    //   o is a 3-bit opcode that stores the current geometry type
    //   (line segment or arc)
    //
    //   a is a 3-bit argument whose interpretation depends on whether
    //   we are a line segment or arc
    //
    //   x and y are 5-bit scene coordinates.
    //
    x+=*b/4&15;
    o=*b&3; a=*++b&7; y=*b/8&7;

    v k={x,y,0}, d={a*(o&1),o/2*a,0};

    if (o) 

      // The current primitive is a line segment extending
      // from (x,y) to (x+dx, y+dy), where:
      //
      //   dx = a if the low bit of o is set, otherwise zero.
      //
      //   dy = a if the middle bit of o is set, otherwise zero.
      //
      // Here, k stores the point (x,y), d stores the vector (dx,dy)
      //

      // Compute the closest point along the line segment and
      // feed it to the distance query. 
      Q(A(k,d,U(O(A(p,k,-1),d)/O(d,d))),F);

    else {

      // The current primitive is an arc centered around
      // (x,y) with starting angle l, ending angle u,
      // and radius r, where
      //
      //   l = -M_PI if the high bit of a is set, otherwise 0.
      //   
      //   u = M_PI if the middle bit of a is set, otherwise 0.
      //
      //   r = 1.5 if the low bit of a is set, otherwise 1.
      //
      // Also compute t, the angle of the point p with respect to the
      // arc's center.
      //
      f r=H*(a&1)+1, t=atan2(p.y-y*H,p.x-x*H),
        P=M_PI, l=-P*(a/4&1), u=P*(a/2&1);
      
      // Clamp t to the range given.
      t = t<l?l:t>u?u:t;

      // Compute the point along the arc closest to p (just given by
      // the angle t and radius r), and feed it to the distance query.
      Q(A(S(k,H),G(cos(t),sin(t),0),r),F);

    } 


  } // Looping though primitives

  N=G(0,1,0); Q(A(p,N,-O(p,N)-.9),W);

  M = M.x==1 && ((int)((p.x+64)/8)^(int)((p.z+64)/8))&1 ?
    G(Y,0,0) : M;

  N=A(P,C,-1);

  //dumpf(__LINE__, I);
  return sqrt(I)-.45;

}

v R(v o, v d, f z) {

#if 1
  checkpoint(__LINE__);

  dumpf(__LINE__, o.x);
  dumpf(__LINE__, o.y);
  dumpf(__LINE__, o.z);

  dumpf(__LINE__, d.x);
  dumpf(__LINE__, d.y);
  dumpf(__LINE__, d.z);

  dumpf(__LINE__, z);
#endif

  f u=0, l=0, i=0, a=1, k=d.z*d.z;
  v p, n;

  // TODO whoa, delete the loop (leave its guts) and the problem goes away
  while (u<97) {
    //checkpoint(__LINE__);
    //f t1 = (l=D(p = A(o,d,u+=l)));
    //dumpf(__LINE__, t1);
  }
  
  //checkpoint(__LINE__);
  return G(k,k,1);

}

int miniray() {

  // Set up the lighting direction and material
  //L=_(G(-1,1,2));
  //F=G(Y,Y,1);
  
      x = -300;

      //checkpoint(__LINE__);
      v p = S( R( G(-2,4,25),
                  _( A( G(10.25,-2,-25) ,
                        A( S( _(G(5,0,2)), x ), 
                           _(G(-2,73,0)),-y), .034) ),
                  2 ), 255 );

  return 0;

}

