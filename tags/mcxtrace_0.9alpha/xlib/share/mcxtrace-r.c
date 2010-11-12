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
* Version: $Revision: $
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
               double kx, double ky, double kz, double phi,
               double Ex, double Ey, double Ez, double p)
{
    double *dptr = &s[11*index];
    *dptr++  = x;
    *dptr++  = y ;
    *dptr++  = z ;
    *dptr++  = kx;
    *dptr++  = ky;
    *dptr++  = kz;
    *dptr++  = phi;
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
               double *kx, double *ky, double *kz, double *phi,
               double *Ex, double *Ey, double *Ez, double *p)
{
    double *dptr = &s[11*index];
    *x  =  *dptr++;
    *y  =  *dptr++;
    *z  =  *dptr++;
    *kx =  *dptr++;
    *ky =  *dptr++;
    *kz =  *dptr++;
    *phi=  *dptr++;
    *Ex =  *dptr++;
    *Ey =  *dptr++;
    *Ez =  *dptr++;
    *p  =  *dptr;
} /* mcrestore_xray */

/*******************************************************************************
* mcsetstate: transfer parameters into global McStas variables 
*******************************************************************************/
void
mcsetstate(double x, double y, double z, double kx, double ky, double kz,
           double phi, double Ex, double Ey, double Ez, double p)
{
  extern double mcnx, mcny, mcnz, mcnkx, mcnky, mcnkz;
  extern double mcnphi, mcnEx, mcnEy, mcnEz, mcnp;

  mcnx = x;
  mcny = y;
  mcnz = z;
  mcnkx = kx;
  mcnky = ky;
  mcnkz = kz;
  mcnphi = phi;
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
  mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
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
  double dl1p=0,dl0p=0,dl1c=0,dl0c=0,y0,y1;
  int ret=1,stat=0,plane_stat=0;
  enum {HIT_CYL=01,ENTER_TOP=02,ENTER_BOT=04,EXIT_TOP=010,EXIT_BOT=020,ENTER_MASK=060,EXIT_MASK=030};
  k2=(kx*kx + ky*ky + kz*kz);
  k=sqrt(k2);

  /*check for prop. vector 0*/
  if(!k2) return 0;

  A= (k2 - ky*ky);
  B= 2*(x*kx + z*kz);
  C=(x*x + z*z - r*r);
  D=B*B-4*A*C;
  if(D>=0){
    if (kx || kz){
      stat|=HIT_CYL;
    /*propagation not parallel to y-axis*/
    /*hit infinitely high cylinder?*/
      D=sqrt(D);
      dl0c=k*(-B-D)/(2*A);
      dl1c=k*(-B+D)/(2*A);
      y0=dl0c*ky/k+y;
      y1=dl1c*ky/k+y;
      if ( (y0<-h/2 && y1<-h/2) || (y0>h/2 && y1>h/2) ){
        /*ray passes above or below cylinder*/
        return 0;
      }
    }
    /*now check top and bottom planes*/
    if (ky){
      dl0p = k*(-h/2-y)/ky;
      dl1p = k*(h/2-y)/ky;
      /*switch solutions?*/
      if (dl0p<dl1p){
        stat|=(ENTER_BOT|EXIT_TOP);
      }else{
        double tmp=dl1p;
        dl1p=dl0p;dl0p=tmp;
        stat|=(ENTER_TOP|EXIT_BOT);
      }
    }
  }
  if (stat & HIT_CYL){
    if (ky && dl0p>dl0c){
      *l0=dl0p;/*1st top/bottom plane intersection happens after 1st cylinder intersect*/
      stat|= plane_stat & ENTER_MASK;
    } else
      *l0=dl0c;
    if(ky && dl1p<dl1c){
      *l1=dl1p;/*2nd top/bottom plane intersection happens before 2nd cylinder intersect*/
      stat|= plane_stat & EXIT_MASK;
    }else
      *l1=dl1c;
  }
  return stat;
} /* cylinder_intersect */

/*******************************************************************************
 * sphere_intersect: Calculate intersection between a line and a sphere.
 * returns 0 when no intersection is found
 *      or 1 in case of intersection with resulting times t0 and t1 
 *******************************************************************************/
int
sphere_intersect(double *l0, double *l1, double x, double y, double z,
                 double kx, double ky, double kz, double r)
{
  double B, C, D, k;

  k = sqrt(kx*kx + ky*ky + kz*kz);
  B = (x*kx + y*ky + z*kz);
  C = x*x + y*y + z*z - r*r;
  D = B*B - k*C;
  if(D < 0)
    return 0;
  D = sqrt(D);
  *l0 = (-B - D) / k;
  *l1 = (-B + D) / k;
  return 1;
} /* sphere_intersect */

/*******************************************************************************
 * plane_intersect: Calculate intersection between a plane (with normal n including the point w)
 * and a line thourhg x along the direction k.
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

#endif /* !MCXTRACE_H */
