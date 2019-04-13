//#include <stdio.h>
#define M_PI 3.141592653589
//#include <math.h>

extern int putchar(int);
extern double sin(double);
extern double cos(double);
extern double sqrt(double);
extern double pow(double, double);
extern double atan2(double, double);

#define E return

typedef float f;
typedef struct { f x,y,z; } v;

f H=.5, Z=.33, Y=.66, I, x, y=-111;
v L, W={1,1,1}, F, P, C, M, N;

v G(f x, f y, f z) { v n={x,y,z}; return n; }
v A(v a, v b, f c) { return G(a.x+b.x*c, a.y+b.y*c, a.z+b.z*c); }
f O(v a, v b) { return a.x*b.x+a.y*b.y+a.z*b.z; }
v S(v a, f s) { return G(a.x*s, a.y*s, a.z*s); }
v _(v a) { return S(a, 1/sqrt(O(a,a))); }

f U(f a) { return a<0?0:a>1?1:a; }

f Q(v c, v m) {
  v D = A(P,c,-1); 
  f d = O(D,D);
  return d<I ? C=c,M=m,I=d:d;
}

f D(v p) {

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

  return sqrt(I)-.45;

}

v R(v o, v d, f z) {

  f u=0, l=0, i=0, a=1, k=d.z*d.z;
  v p, n;

  while (u<97) 
    if ((l=D(p = A(o,d,u+=l)))*l<.001) {
      
      p=M;
      n=_(N);
          
      // Ok, we have intersected! Now compute lighting, which consists
      // of three terms: a Lambertian term, a specular term, and ambient
      // occlusion.
          
      // update o to put it at the point of intersection
      o=A(o,d,u);
          
      // It's time to compute ambient occlusion now. At this point it
      // would be instructive to look at the linked PDF here:
      //
      //   http://iquilezles.org/www/material/nvscene2008/nvscene2008.htm
      //
      // Basically, we're going to use the distance function D to get an
      // idea of how much "clutter" there is as we march away from the 
      // intersecting point along the normal n.  
      //
      // Take 5 steps along the normal:
      while (++i<6)  

        // OK, we are comparing two distances here:
        //
        //   .2*i is the distance along the normal from the point of
        //   intersection.
        //
        //   The D(...) subexpression is the distance to closest point
        //   in the scene. 
        //
        // If the point of intersection was the closest thing, then
        // the difference below should be zero and there is no
        // occlusion happening.
        //
        // However, if there's non-convex geometry in that region, the
        // D(...) thing will be less than the .2*i thing and we end up
        // contributing to a. Each time we take a step along the
        // normal, we reduce the "importance" of the AO computation by
        // a factor of 2 (occlusion at larger distances matter
        // exponentially less).
        //
        a -= U(i/5-D(A(o,n,i/5)))/pow(2,i);

      // Do all the lighting now:
      //
      //   v(m,m,1) is the color (white or blue)
      //
      //   U(n%L)/3+.65 is a blend of 0.33 Lambertian and 0.65
      //   ambient, and modulates the color.
      //
      //   Finally we take that entire thing and darken it up with
      //   the ambient occlusion term that we computed above.
      p = S(p,(U(O(n,L))*Z+Y)*a);
      
      p = z?A(S(p,Y),R(A(o,n,.1),A(d,n,-2*O(d,n)),z-1),Z):p;

      // Compute a specular term and use it to linearly interpolate
      // between the surface color and white; this is physically
      // implausible, but doesn't require clamping (as does, for
      // instance, the Phong shading model).
      u=pow(U(O(n,_(A(L,d,-1)))),40);

      return A(S(p,1-u),W,u);

    }
  
  return G(k,k,1);

}

int miniray() {

  // Set up the lighting direction and material
  L=_(G(-1,1,2));
  F=G(Y,Y,1);
  
  // Output the portable pixmap header (binary version).
  {
    char *p = "P6 600 220 255";
    while (*p)
    {
        putchar(*p);
        p++;
    }
    putchar(10);
  }
  
  // For each row:
  // y holds the vertical pixel coordinate
  while (++y<110) 
    
    // For each column:
    // x holds the horizontal pixel coordinate
    for (x=-300;x<300;++x) {
      
      v p = S( R( G(-2,4,25),
                  _( A( G(10.25,-2,-25) ,
                        A( S( _(G(5,0,2)), x ), 
                           _(G(-2,73,0)),-y), .034) ),
                  2 ), 255 );
      putchar((int) p.x);
      putchar((int) p.y);
      putchar((int) p.z);

    }

  return 0;

}

