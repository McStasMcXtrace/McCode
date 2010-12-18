#include <cmath>

/* solve_2nd_order: second order equation solve: A*t^2 + B*t + C = 0
 * returns 0 if no solution was found, or set 't' to the smallest positive
 * solution.
 * EXAMPLE usage for intersection of a trajectory with a plane in gravitation
 * field (gx,gy,gz):
 * The neutron starts at point r=(x,y,z) with velocityv=(vx vy vz). The plane
 * has a normal vector n=(nx,ny,nz) and contains the point W=(wx,wy,wz).
 * The problem consists in solving the 2nd order equation:
 *      1/2.n.g.t^2 + n.v.t + n.(r-W) = 0
 * so that A = 0.5 n.g; B = n.v; C = n.(r-W);
 * Without acceleration, t=-n.(r-W)/n.v
 */
int solve_2nd_order(double *Idt,
                  double A,  double B,  double C)
{
  int ret=0;

  *Idt = 0;

  if (fabs(A) < 1E-10) /* this plane is parallel to the acceleration: A ~ 0 */
  {
    if (B) {  *Idt = -C/B; ret=3; }
    /* else the speed is parallel to the plane, no intersection: A=B=0 ret=0 */
  }
  else
  {
    double D;
    D = B*B - 4*A*C;
    if (D >= 0) /* Delta > 0: neutron trajectory hits the mirror */
    {
      double sD, dt1, dt2;
      sD = sqrt(D);
      dt1 = (-B + sD)/2/A;
      dt2 = (-B - sD)/2/A;
      /* we identify very small values with zero */
      if (fabs(dt1) < 1e-10) dt1=0.0;
      if (fabs(dt2) < 1e-10) dt2=0.0;

      /* now we choose the smallest positive solution */
      if      (dt1<=0.0 && dt2>0.0) ret=2; /* dt2 positive */
      else if (dt2<=0.0 && dt1>0.0) ret=1; /* dt1 positive */
      else if (dt1> 0.0 && dt2>0.0)
      {  if (dt1 < dt2) ret=1; else ret=2; } /* all positive: min(dt1,dt2) */
      /* else two solutions are negative. ret=0 */
      if (ret==1) *Idt = dt1; else if (ret==2) *Idt=dt2;
    } /* else Delta <0: no intersection.  ret=0 */
  }
  return(ret);
}

