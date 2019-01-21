/* 
 * OpenCL Kernel files
 *
 * hkl_search
 *  search the HKL reflections which are on the Ewald sphere
 *  input:
 *     L,T,count,V0: constants for all calls
 *     kix,kiy,kiz: may be different for each call
 *  this function returns:
 *     tau_count (return), coh_refl, coh_xsect, T (updated elements in the array up to [j]) -----output
 * Created by Jinyan LIU
 * Date: Juin 2014
 */

#pragma OPENCL EXTENSION cl_khr_fp64 : enable


#define PI 3.14159265358979323846

typedef struct{
    int h,k,l;                  /* Indices for this reflection */
    double F2;                  /* Value of structure factor */
    double tau_x, tau_y, tau_z; /* Coordinates in reciprocal space */
    double tau;                 /* Length of (tau_x, tau_y, tau_z) */
    double u1x, u1y, u1z;       /* First axis of local coordinate system */
    double u2x, u2y, u2z;       /* Second axis of local coordinate system */
    double u3x, u3y, u3z;       /* Third axis of local coordinate system */
    double sig123;              /* The product sig1*sig2*sig3 = volume of spot */
    double m1, m2, m3;          /* Diagonal matrix representation of Gauss */
    double cutoff;              /* Cutoff value for Gaussian tails */
} hkl_data;/* L */

typedef struct{
      int index;                  /* Index into reflection table */
      double refl;
      double xsect;
      /* The following vectors are in local koordinates. */
      double rho_x, rho_y, rho_z; /* The vector ki - tau */
      double rho;                 /* Length of rho vector */
      double ox, oy, oz;          /* Origin of Ewald sphere tangent plane */
      double b1x, b1y, b1z;       /* Spanning vectors of Ewald sphere tangent */
      double b2x, b2y, b2z;
      double l11, l12, l22;       /* Cholesky decomposition L of 2D Gauss */
      double y0x, y0y;            /* 2D Gauss center in tangent plane */
} tau_data;/* T */


/* simple vector algebra ==================================================== */

void vec_prod(double x, double y, double z,
		double x1, double y1, double z1,
		double x2, double y2, double z2) {
    x = (y1)*(z2) - (y2)*(z1);
    y = (z1)*(x2) - (z2)*(x1);
    z = (x1)*(y2) - (x2)*(y1);
}/* vec_prod */

/* normal_vec: Compute normal vector to (x,y,z). */
void normal_vec(double *nx, double *ny, double *nz,
                double x, double y, double z)
{
  double ax = fabs(x);
  double ay = fabs(y);
  double az = fabs(z);
  double l;
  if(x == 0 && y == 0 && z == 0)
  {
    *nx = 0;
    *ny = 0;
    *nz = 0;
    return;
  }
  if(ax < ay)
  {
    if(ax < az)
    {                           /* Use X axis */
      l = sqrt(z*z + y*y);
      *nx = 0;
      *ny = z/l;
      *nz = -y/l;
      return;
    }
  }
  else
  {
    if(ay < az)
    {                           /* Use Y axis */
      l = sqrt(z*z + x*x);
      *nx = z/l;
      *ny = 0;
      *nz = -x/l;
      return;
    }
  }
  /* Use Z axis */
  l = sqrt(y*y + x*x);
  *nx = y/l;
  *ny = -x/l;
  *nz = 0;
} /* normal_vec */


////////////////////////////////////////////////////////////////////////////////
// OpenCL Kernel for hkl_search
////////////////////////////////////////////////////////////////////////////////

__kernel void hkl_search(
    __global hkl_data *d_L,             //input(read_only,allocate memory 1 time) 
    __global tau_data *d_T,             //output(read_and_write,allocate memory 1 time)
    int count,                          //input
    const double V0,                    //input
    double kix,                         //input
    double kiy,                         //input    
    double kiz,                         //input
    const double tau_max,               //input
    __global int *d_tau_count,          //return value(number of reachable reflections)
    __global double *coh_refl,          //output(initial0)
    __global double *coh_xsect)        //output(initial0)
  {
    double rho, rho_x, rho_y, rho_z;
    double diff;
    int    i0;
    
    //int    j = get_global_id(1) ;    /* Index into reflection list */
    double ox,oy,oz;
    double b1x,b1y,b1z, b2x,b2y,b2z, kx, ky, kz, nx, ny, nz ;
    double n11, n22, n12, det_N, inv_n11, inv_n22, inv_n12, l11, l22, l12, det_L;
    double Bt_D_O_x, Bt_D_O_y, y0x, y0y, alpha;
    double ki = sqrt(kix*kix+kiy*kiy+kiz*kiz);
    double xsect_factor = pow(2*PI, 5.0/2.0)/(V0*ki*ki);
    
    for(i0 = 0; i0 < count; i0+=4096)   //4096 global work size
      {    
        int    i = get_global_id(0) + i0 ;
        int    j = 0;
    /* Common factor in coherent cross-section */

        if(i < count)
          {
    /* Assuming reflections are sorted, stop search when max tau exceeded. */
            if(d_L[i].tau <= tau_max)
            {
              /* Check if this reciprocal lattice point is close enough to the
                 Ewald sphere to make scattering possible. */
              rho_x = kix - d_L[i].tau_x;
              rho_y = kiy - d_L[i].tau_y;
              rho_z = kiz - d_L[i].tau_z;
              rho = sqrt(rho_x*rho_x + rho_y*rho_y + rho_z*rho_z);
              diff = fabs(rho - ki);

              /* Check if scattering is possible (cutoff of Gaussian tails). */
              if(diff <= d_L[i].cutoff)
              {
                j++;
                /* Store reflection. */
                d_T[j].index = i;
                /* Get ki vector in local coordinates. */
                kx = kix*d_L[i].u1x + kiy*d_L[i].u1y + kiz*d_L[i].u1z;
                ky = kix*d_L[i].u2x + kiy*d_L[i].u2y + kiz*d_L[i].u2z;
                kz = kix*d_L[i].u3x + kiy*d_L[i].u3y + kiz*d_L[i].u3z;
                d_T[j].rho_x = kx - d_L[i].tau;
                d_T[j].rho_y = ky;
                d_T[j].rho_z = kz;
                d_T[j].rho = rho;
                /* Compute the tangent plane of the Ewald sphere. */
                
                nx = d_T[j].rho_x/d_T[j].rho;
                ny = d_T[j].rho_y/d_T[j].rho;
                nz = d_T[j].rho_z/d_T[j].rho;
                ox = (ki - d_T[j].rho)*nx;
                oy = (ki - d_T[j].rho)*ny;
                oz = (ki - d_T[j].rho)*nz;
                d_T[j].ox = ox;
                d_T[j].oy = oy;
                d_T[j].oz = oz;
                                
                /* Compute unit vectors b1 and b2 that span the tangent plane. */
                normal_vec(&b1x, &b1y, &b1z, nx, ny, nz);
                vec_prod(b2x, b2y, b2z, nx, ny, nz, b1x, b1y, b1z);
                d_T[j].b1x = b1x;
                d_T[j].b1y = b1y;
                d_T[j].b1z = b1z;
                d_T[j].b2x = b2x;
                d_T[j].b2y = b2y;
                d_T[j].b2z = b2z;
                /* Compute the 2D projection of the 3D Gauss of the reflection. */
                /* The symmetric 2x2 matrix N describing the 2D gauss. */
                n11 = d_L[i].m1*b1x*b1x + d_L[i].m2*b1y*b1y + d_L[i].m3*b1z*b1z;
                n12 = d_L[i].m1*b1x*b2x + d_L[i].m2*b1y*b2y + d_L[i].m3*b1z*b2z;
                n22 = d_L[i].m1*b2x*b2x + d_L[i].m2*b2y*b2y + d_L[i].m3*b2z*b2z;
                /* The (symmetric) inverse matrix of N. */
                det_N = n11*n22 - n12*n12;
                inv_n11 = n22/det_N;
                inv_n12 = -n12/det_N;
                inv_n22 = n11/det_N;
                /* The Cholesky decomposition of 1/2*inv_n (lower triangular L). */
                l11 = sqrt(inv_n11/2);
                l12 = inv_n12/(2*l11);
                l22 = sqrt(inv_n22/2 - l12*l12);
                d_T[j].l11 = l11;
                d_T[j].l12 = l12;
                d_T[j].l22 = l22;
                det_L = l11*l22;
                /* The product B^T D o. */
                Bt_D_O_x = b1x*d_L[i].m1*ox + b1y*d_L[i].m2*oy + b1z*d_L[i].m3*oz;
                Bt_D_O_y = b2x*d_L[i].m1*ox + b2y*d_L[i].m2*oy + b2z*d_L[i].m3*oz;
                /* Center of 2D Gauss in plane coordinates. */
                y0x = -(Bt_D_O_x*inv_n11 + Bt_D_O_y*inv_n12);
                y0y = -(Bt_D_O_x*inv_n12 + Bt_D_O_y*inv_n22);
                d_T[j].y0x = y0x;
                d_T[j].y0y = y0y;
                /* Factor alpha for the distance of the 2D Gauss from the origin. */
                alpha = d_L[i].m1*ox*ox + d_L[i].m2*oy*oy + d_L[i].m3*oz*oz -
                             (y0x*y0x*n11 + y0y*y0y*n22 + 2*y0x*y0y*n12);
                d_T[j].refl  = xsect_factor*det_L*exp(-alpha)/d_L[i].sig123;  /* intensity of that Bragg */
                *coh_refl   += d_T[j].refl;                                    /* total scatterable intensity */
                d_T[j].xsect = d_T[j].refl*d_L[i].F2;
                *coh_xsect  += d_T[j].xsect;
                *d_tau_count+=j;
                }
              }
              else i0=count; // break
          }
      } /* end for */
        //return (j); // this is 'd_tau_count', i.e. number of reachable reflections
    } /* end hkl_search */
