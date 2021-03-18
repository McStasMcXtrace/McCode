/*******************************************************************************
*
* McXtrace, X-ray tracing package
*           Copyright (C) 1997-2009, All rights reserved
*           Risoe National Laboratory, Roskilde, Denmark
*           Institut Laue Langevin, Grenoble, France
*
* Runtime: share/mcxtrace-r.c
*
* %Identification
* Edited by: EK
* Date:    May 29, 2009
* Release: McXtrace X.Y
* Version: $Revision$
*
* Runtime system for McXtrace.
* Embedded within instrument in runtime mode.
*
* Usage: Automatically embedded in the c code whenever required.
*
*******************************************************************************/

#ifndef MCXTRACE_H

/*******************************************************************************
* mcstore_xray: stores neutron coodinates into global array (per component)
*******************************************************************************/
void
mcstore_xray(MCNUM *s, int index, double x, double y, double z,
               double kx, double ky, double kz, double phi, double t,
               double Ex, double Ey, double Ez, double p)
{
    double *dptr = &s[12*index];
    *dptr++  = x;
    *dptr++  = y ;
    *dptr++  = z ;
    *dptr++  = kx;
    *dptr++  = ky;
    *dptr++  = kz;
    *dptr++  = phi;
    *dptr++  = t;
    *dptr++  = Ex;
    *dptr++  = Ey;
    *dptr++  = Ez;
    *dptr    = p ;
}

/*******************************************************************************
* mcrestore_xray: restores neutron coodinates from global array
*******************************************************************************/
void
mcrestore_xray(MCNUM *s, int index, double *x, double *y, double *z,
               double *kx, double *ky, double *kz, double *phi, double *t,
               double *Ex, double *Ey, double *Ez, double *p)
{
    double *dptr = &s[12*index];
    *x  =  *dptr++;
    *y  =  *dptr++;
    *z  =  *dptr++;
    *kx =  *dptr++;
    *ky =  *dptr++;
    *kz =  *dptr++;
    *phi=  *dptr++;
    *t  =  *dptr++;
    *Ex =  *dptr++;
    *Ey =  *dptr++;
    *Ez =  *dptr++;
    *p  =  *dptr;
} /* mcrestore_xray */

/*******************************************************************************
* mcsetstate: transfer parameters into global McXtrace variables 
*******************************************************************************/
void
mcsetstate(double x, double y, double z, double kx, double ky, double kz,
           double phi, double t, double Ex, double Ey, double Ez, double p)
{
  extern double mcnx, mcny, mcnz, mcnkx, mcnky, mcnkz;
  extern double mcnphi, mcnt, mcnEx, mcnEy, mcnEz, mcnp;

  mcnx = x;
  mcny = y;
  mcnz = z;
  mcnkx = kx;
  mcnky = ky;
  mcnkz = kz;
  mcnphi = phi;
  mcnt = t;
  mcnEx = Ex;
  mcnEy = Ey;
  mcnEz = Ez;
  mcnp = p;
} /* mcsetstate */

/*******************************************************************************
* mcgenstate: set default xray parameters 
*******************************************************************************/
void
mcgenstate(void)
{
  mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1);
  /* old initialisation: mcsetstate(0, 0, 0, 0, 0, 1, 0, sx=0, sy=1, sz=0, 1); */
}

/* intersection routines ==================================================== */

/*******************************************************************************
* inside_rectangle: Check if (x,y) is inside rectangle (xwidth, yheight) 
* return 0 if outside and 1 if inside 
*******************************************************************************/
int inside_rectangle(double x, double y, double xwidth, double yheight)
{
  if (x>-xwidth/2 && x<xwidth/2 && y>-yheight/2 && y<yheight/2)
    return 1;
  else
    return 0;
}

/*******************************************************************************
 * box_intersect: compute length intersection with a box
 * returns 0 when no intersection is found
 *      or 1 in case of intersection with resulting travelling lengths dl_in and dl_out
*******************************************************************************/
int box_intersect(double *dl_in, double *dl_out,
                  double x, double y, double z,
                  double kx, double ky, double kz,
                  double dx, double dy, double dz)
{

  double k, l,xf,yf,zf, l_[6],dx_2,dy_2,dz_2;
  double ab[2];
  unsigned int count=0;
  k=sqrt(scalar_prod(kx,ky,kz,kx,ky,kz));
  dx_2=dx/2.0;dy_2=dy/2.0;dz_2=dz/2.0; 
  /*we really don't need to store the 6 intersects as only two are possible. i.e. should remove that.*/
  if (kx) {
    l=(-dx_2-x)/kx*k;
    yf=l*ky/k+y;zf=l*kz/k+z;
    if(yf > -dy_2 && yf<dy_2 && zf > -dz_2 && zf<dz_2){
      l_[0]=l;
      ab[count++]=l_[0];
    }else{
      l_[0]=0;
    }
    l=(dx_2-x)/kx*k;
    yf=l*ky/k+y;zf=l*kz/k+z;
    if(yf > -dy_2 && yf<dy_2 && zf > -dz_2 && zf<dz_2){
      l_[1]=l;
      ab[count++]=l_[1];
    }else{
      l_[1]=0;
    }
  }
  if (ky) {
    l=(-dy_2-y)/ky*k;
    xf=l*kx/k+x;zf=l*kz/k+z;
    if(xf > -dx_2 && xf<dx_2 && zf > -dz_2 && zf<dz_2){
      l_[2]=l;
      ab[count++]=l_[2];
    }else{
      l_[2]=0;
    } 
    l=(dy_2-y)/ky*k;
    xf=l*kx/k+x;zf=l*kz/k+z;
    if(xf > -dx_2 && xf<dx_2 && zf > -dz_2 && zf<dz_2){
      l_[3]=l;
      ab[count++]=l_[3];
    }else{
      l_[3]=0;
    }
  }
  if (kz) {
    l=(-dz_2-z)/kz*k;
    xf=l*kx/k+x; yf=l*ky/k+y;
    if(xf > -dx_2 && xf<dx_2 && yf > -dy_2 && yf<dy_2){
      l_[4]=l;
      ab[count++]=l_[4];
    }else{
      l_[4]=0;
    }
    l=(dz_2-z)/kz*k;
    xf=l*kx/k+x; yf=l*ky/k+y;
    if(xf > -dx_2 && xf<dx_2 && yf > -dy_2 && yf<dy_2){
      l_[5]=l;
      ab[count++]=l_[5];
    }else{
      l_[5]=0;
    }
  }
  /*check validity of intersects*/
  if (count>2){
    fprintf(stderr,"box_instersect: xray hitting box more than twice\n");
  }
  if (!count){
    *dl_in=0;*dl_out=0;
    return 0;
  }

  if (ab[0]<ab[1]){
    *dl_in=ab[0];*dl_out=ab[1];
    return 1;
  }else{
    *dl_in=ab[1];*dl_out=ab[0];
    return 1;
  }
} /* box_intersect */

/*******************************************************************************
 * cylinder_intersect: compute intersection with a cylinder
 * returns 0 when no intersection is found
 *      or 1/2/4/8/16 bits depending on intersection,
 *     and resulting times l0 and l1
 * Written by: EK 11.6.09 
 *******************************************************************************/
int
cylinder_intersect(double *l0, double *l1, double x, double y, double z,
                   double kx, double ky, double kz, double r, double h)
{
  double A,B,C,D,k2,k;
  double dl1=0,dl0=0,y0,y1;
  int rval=1;
  k2=(kx*kx + ky*ky + kz*kz);
  k=sqrt(k2);

  /*check for prop. vector 0*/
  if(fabs(k2)<DBL_EPSILON) return 0;

  A= (kx*kx+kz*kz);
  B= 2*(x*kx + z*kz);
  C=(x*x + z*z - r*r);
  D=B*B-4*A*C;
  if(D>=0){
    if (A){
      /*propagation not parallel to y-axis*/
      /*hit infinitely high cylinder?*/
      D=sqrt(D);
      /*stabilize these solutions according to numerical recipes*/
      if(B>=0){
        dl0 = (-B-D)/(2*A);
        dl1 = (C/(A*dl0));
        dl1*=k;dl0*=k;
      }else{
        dl1 = (-B+D)/(2*A);
        dl0 = (C/(A*dl1));
        dl1*=k;dl0*=k;
      }
    }else if (ky){
      /*propagation is parallel to y-axis*/
      /*check if we will hit cylinder top/bottom at all*/
      dl0 = k*(-h/2-y)/ky;
      dl1 = k*(h/2-y)/ky;
      /*switch solutions?*/
      if (dl0>dl1){
        double tmp=dl1;
        dl1=dl0;dl0=tmp;
      }
    }
    /*dl0,dl1 now contains either plane hits or cylinder hits
      We determine which is which by checks on the corresponding y-ccordinate: y0,y1*/
    y0=ky*dl0/k + y;
    y1=ky*dl1/k + y;

    /*if both are strictly > than h/2.0 we have missed the cylinder completely*/
    if( (y0>h/2.0 && y1 >h/2.0) || (y0<-h/2.0 && y1<-h/2.0) ) {
      *l0=0; *l1=0; return 0;
    }

    if( y0 > h/2.0 ) {
      dl0 = k*(h/2-y)/ky; rval +=2;
    }else if ( y0 < -h/2.0 ){
      dl0 = k*(-h/2-y)/ky; rval +=4;
    }
    if( y1 > h/2.0 ) {
      dl1 = k*(h/2-y)/ky; rval +=8;
    }else if ( y1 < -h/2.0 ){
      dl1 = k*(-h/2-y)/ky; rval +=16;
    }

    *l0=dl0;
    *l1=dl1;
    return rval;
  }else{
    /* no solution found - i.e. D<0 happens when we completely miss the infinite cylinder*/
    *l0=0;
    *l1=0;
    return 0;
  }
} /* cylinder_intersect */

/*******************************************************************************
 * sphere_intersect: Calculate intersection between a line and a sphere.
 * returns 0 when no intersection is found
 *      or 1 in case of intersection with resulting lengths l0 and l1 
 *******************************************************************************/
int
sphere_intersect(double *l0, double *l1, double x, double y, double z,
                 double kx, double ky, double kz, double r){
  double B, C, D, A;

  A = kx*kx + ky*ky + kz*kz;
  B = (x*kx + y*ky + z*kz);
  C = x*x + y*y + z*z - r*r;
  D = B*B - A*C;
  if(D < 0 || A==0 )
    return 0;
  D = sqrt(D);
  if (B>=0){
    *l0 = (-B - D) / A;
    *l1 = C/(*l0 *A);
  }else{
    *l0 = C /(-B + D);
    *l1 = C /(*l0 *A);
  }
  *l0*=sqrt(A); *l1*=sqrt(A);
  return 1;
} /* sphere_intersect */

/******************************************************************************
 * ellipsoid_intersect: Calculate intersection between a line and an ellipsoid.
 * They ellisoid is fixed by a set of half-axis (a,b,c) and a matrix Q, with the
 * columns of Q being the (orthogonal) vectors along which the half-axis lie.
 * This allows for complete freedom in orienting the ellipsoid.
 * returns 0 when no intersection is found
 *      or 1 when they _are_ found with resulting lengths l0 and l1.
 *****************************************************************************/
int
ellipsoid_intersect(double *l0, double *l1, double x, double y, double z,
    double kx, double ky, double kz, double a, double b, double c,
    Rotation Q)
{
  Rotation A,Gamma,Q_t,Tmp;
  double u,v,w;

  Gamma[0][0]=Gamma[0][1]=Gamma[0][2]=0;
  Gamma[1][1]=Gamma[1][0]=Gamma[1][2]=0;
  Gamma[2][2]=Gamma[2][0]=Gamma[2][1]=0;
  /* now set diagonal to ellipsoid half axis if non-zero.
   * This way a zero value means the ellipsoid extends infinitely along that axis,
   * which is useful for objects only curved in one direction*/ 
  if (a!=0){
    Gamma[0][0]=1/(a*a);
  }
  if (b!=0){
    Gamma[1][1]=1/(b*b);
  }
  if (c!=0){
    Gamma[2][2]=1/(c*c);
  }

  if (Q!=NULL){
    rot_transpose(Q,Q_t);
    rot_mul(Gamma,Q_t,Tmp);
    rot_mul(Q,Tmp,A);
  }else{
    rot_copy(A,Gamma);
  }

  /*to get the solutions as lengths in m use unit vector along k*/
  double ex,ey,ez,k;
  k=sqrt(kx*kx+ky*ky+kz*kz);
  ex=kx/k;
  ey=ky/k;
  ez=kz/k;

  u=ex*(A[0][0]*ex + A[1][0]*ey + A[2][0]*ez) + ey*( A[0][1]*ex + A[1][1]*ey + A[2][1]*ez) + ez*(A[0][2]*ex + A[1][2]*ey + A[2][2]*ez);
  v=x *(A[0][0]*ex + A[1][0]*ey + A[2][0]*ez) + ex*(A[0][0]*x + A[1][0]*y + A[2][0]*z) +
    y *(A[0][1]*ex + A[1][1]*ey + A[2][1]*ez) + ey*(A[0][1]*x + A[1][1]*y + A[2][1]*z) +
    z *(A[0][2]*ex + A[1][2]*ey + A[2][2]*ez) + ez*(A[0][2]*x + A[1][2]*y + A[2][2]*z);
  w=x*(A[0][0]*x + A[1][0]*y + A[2][0]*z) + y*(A[0][1]*x + A[1][1]*y + A[2][1]*z) + z*(A[0][2]*x + A[1][2]*y + A[2][2]*z);

  double D=v*v-4*u*w+4*u;
  if (D<0) return 0;

  D=sqrt(D);

  *l0=(-v-D) / (2*u);
  *l1=(-v+D) / (2*u);
  return 1;
}


/*******************************************************************************
 * plane_intersect: Calculate intersection between a plane (with normal n including the point w)
 * and a line through x along the direction k.
 * returns 0 when no intersection is found (i.e. line is parallel to the plane)
 * returns 1 or -1 when intersection length is positive and negative, respectively
 *******************************************************************************/
int
plane_intersect(double *l, double x, double y, double z,
                 double kx, double ky, double kz, double nx, double ny, double nz, double wx, double wy, double wz)
{
  double s,k2;
  k2=scalar_prod(kx,ky,kz,kx,ky,kz);
  s=scalar_prod(kx,ky,kz,nx,ny,nz);
  if (k2<FLT_EPSILON || fabs(s)<FLT_EPSILON) return 0;
  *l = - sqrt(k2)*scalar_prod(nx,ny,nz,x-wx,y-wy,z-wz)/s;
  if (*l<0) return -1;
  else return 1;
} /* plane_intersect */

/*******************************************************************************
 * paraboloid_intersect: Calculate intersection between a rotational paraboloid
 * and a line with direction k through the point x,y,z.
 * The paraboloid is oriented such that it opens towards positive y,with the scaling
 * prms  a and b for x and y resp.. The paex is on the z=0-axis. I.e. the equation for the paraboloid is:
 * z=(x/a)^2 + (y/b)^2
 * If the other direction (opening towards z<0) is wanted simply set the sign parameter to -1
 *******************************************************************************/
int
paraboloid_intersect(double *l0, double *l1, double x, double y, double z,
    double kx, double ky, double kz, double a, double b, int sign)
{
  double A,B,C,D,k;
  double a2i,b2i;
  int retval=0;
  if(a!=0 && b!=0){
    a2i=1.0/(a*a);b2i=1.0/(b*b);
  }else if (a!=0){
    /*paraboloid is infinite/invariant along y, must check if k||y */
    if (kx==0 && kz==0){
      *l0=*l1=0;return 0;
    }
    a2i=1.0/(a*a);b2i=0;
  }else if (b!=0){
    /*paraboloid is infinite/invariant along x, must check if k||x */
    if (ky==0 && kz==0){
      *l0=*l1=0;return 0;
    }
    a2i=0;b2i=1.0/(b*b);
  }
  k=sqrt(kx*kx + ky*ky + kz*kz);

  A=sign*(kx*kx*a2i + ky*ky*b2i);
  B=sign*2*kx*x*a2i + sign*2*ky*y*b2i - kz;
  C=sign*x*x*a2i + sign*y*y*b2i - z;

  retval=solve_2nd_order(l0,l1,A,B,C);
  /*convert to solution in m*/
  *l0 *= k; *l1 *=k;
  return retval;
}

/******************************************************************************
 * paraboloid_normal: Calucate the normal vector to the given paraboloid at the
 * point (x,y,z) and put the result i nx,ny,nz. No check is performed if the
 * the point is on th surface. In that case the result is undefined.
 *****************************************************************************/
int paraboloid_normal(double *nx, double *ny, double *nz, double x,double y, double z, double a, double b, int sign){
  double a2i,b2i;
  double tx,ty,tz;
  if(a!=0 && b!=0){
    a2i=1.0/(a*a);b2i=1.0/(b*b);
  }else if (a!=0){
    a2i=1.0/(a*a);b2i=0;
  }else if (b!=0){
    a2i=0;b2i=1.0/(b*b);
  }else{
    *nx=0; *ny=0; *nz=1;
    return 1;
  }
  tx=-sign*2*x*a2i;
  ty=-sign*2*y*b2i;
  tz=1;
  NORM(tx,ty,tz);
  *nx=tx; *ny=ty; *nz=tz;
  return 1;
}



#endif /* !MCXTRACE_H */
