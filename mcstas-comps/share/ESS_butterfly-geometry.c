/* MCDISPLAY-section for the ESS butterfly moderator */

/* NOTA BENE:
   From McStas 3, this file should be included in %SHARE due to requirement  from clang / macOS, that dislikes function definitons in DISPLAY.
   (clang is more c99-strict than e.g. gcc)

   For this reason, the geometry-calls e.g. line() have to be explicitly the mcdis_line() variant */


/* Define the positioning of the buttefly sketch */

/* Point sets for the butterfly */
double butterfly_z[] = {-1.9922764e-01, -1.8484553e-01, -2.0252845e-01, -2.0795122e-01, -2.1054471e-01, -2.1030894e-01, -2.0889431e-01, -2.0535772e-01, -2.0134959e-01, -1.9639837e-01, -1.9026829e-01, -1.8390244e-01, -1.7565041e-01, -1.7093496e-01, -1.4617886e-01, -1.2873171e-01, -9.2658537e-02, -4.0552845e-02, -1.7682927e-02, -9.1951221e-03, -2.3577231e-03, 5.1869889e-03, 1.1788619e-02, 1.7918699e-02, 2.3105689e-02, 2.4991869e-02, 2.4756099e-02, 2.2162599e-02, 1.8154469e-02, 1.2731709e-02, 5.6585389e-03, -2.3577214e-04, 1.4146339e-02, -2.9707317e-02, 1.4146339e-02, -4.7154514e-04, 1.5325199e-02, 2.1691059e-02, 2.4991869e-02, 2.4991869e-02, 2.2634149e-02, 1.8154469e-02, 1.1552849e-02, 3.3008089e-03, -6.1300811e-03, -9.1951221e-03, -6.0593496e-02, -9.2658537e-02, -1.7541463e-01, -1.8508130e-01, -1.9309756e-01, -1.9899187e-01, -2.0182114e-01, -2.0582927e-01, -2.0913008e-01, -2.1078049e-01, -2.1007317e-01, -2.0630081e-01, -2.0229268e-01, -1.9828455e-01, -1.8484553e-01, -1.9922764e-01, -1.5584553e-01, -1.9922764e-01};
#pragma acc declare create(butterfly_z)

double butterfly_x[] = {1.4279319e-02, 3.1692034e-10, -1.7654432e-02, -2.5962400e-02, -3.4789615e-02, -4.3876452e-02, -5.1665172e-02, -5.8934642e-02, -6.4646372e-02, -6.9059982e-02, -7.2694722e-02, -7.5031332e-02, -7.6589082e-02, -7.6589082e-02, -7.6848702e-02, -7.6589082e-02, -7.6589082e-02, -7.6589082e-02, -7.6589082e-02, -7.6848702e-02, -7.5290962e-02, -7.2694722e-02, -6.8281112e-02, -6.1790512e-02, -5.3482542e-02, -4.4136082e-02, -3.3491495e-02, -2.5443152e-02, -1.9212176e-02, -1.2981200e-02, -6.2309757e-03, -2.5962368e-04, 1.4538943e-02, 5.8415398e-02, 1.0229185e-01, 1.1709042e-01, 1.3266786e-01, 1.4149508e-01, 1.5213966e-01, 1.6200537e-01, 1.7135184e-01, 1.7888093e-01, 1.8589078e-01, 1.9082364e-01, 1.9290063e-01, 1.9341988e-01, 1.9341988e-01, 1.9341988e-01, 1.9341988e-01, 1.9186213e-01, 1.8822740e-01, 1.8459266e-01, 1.8069830e-01, 1.7602507e-01, 1.6849597e-01, 1.5811101e-01, 1.4928380e-01, 1.3993733e-01, 1.3370636e-01, 1.2981200e-01, 1.1709042e-01, 1.0229185e-01, 5.8415398e-02, 1.4279319e-02};
#pragma acc declare create(butterfly_x)

double butterfly_e_z1[]= {-3.0488e-04,  -5.5017e-02, -3.0488e-04};
#pragma acc declare create(butterfly_e_z1)
double butterfly_e_x1[]= {-4.3103e-04,   5.8521e-02,  1.1701e-01};
#pragma acc declare create(butterfly_e_x1)
double butterfly_e_z2[]= {-1.8501e-01, -1.2719e-01, -1.8501e-01};
#pragma acc declare create(butterfly_e_z2)
double butterfly_e_x2[]= {3.3156e-05,  5.8985e-02,  1.1701e-01};
#pragma acc declare create(butterfly_e_x2)

void butterfly_geometry(double Bdelta_y, int Bjmax, double Bcx, double Bcz,
  double Borientation_angle, double *BBeamlines, double Btx, double Bty, double Btz,
  double BrC1_x, double BrC1_z, 
  double BrC2_x, double BrC2_z, 
  double BrC3_x, double BrC3_z,  
  double BrT1_x, double BrT1_z, 
  double BrT2_x, double BrT2_z, 
  double BrT3_x, double BrT3_z,
  double Br11, double Br12, double Br21, double Br22,
  double Bfoc_XW, double Bfoc_YH)
{
  /* Draw the two butterfly shapes at top and bottom level */
  double y0;
  int j;
  double rAx,rAz,rBx,rBz;

  for (y0=-Bdelta_y; y0<2*Bdelta_y; y0+=2*Bdelta_y) {
    for (j=0; j<63; j++) {
      
      rAx = Br11*(butterfly_z[j]-Bcz) + Br12*(butterfly_x[j]-Bcx);
      rAz = Br21*(butterfly_z[j]-Bcz) + Br22*(butterfly_x[j]-Bcx);

      rBx = Br11*(butterfly_z[j+1]-Bcz) + Br12*(butterfly_x[j+1]-Bcx);
      rBz = Br21*(butterfly_z[j+1]-Bcz) + Br22*(butterfly_x[j+1]-Bcx);

      mcdis_line(rAx, y0, rAz, rBx, y0, rBz);

    }
  }

  /* Draw the "border" between the thermal and cold areas */
  for (y0=-Bdelta_y; y0<2*Bdelta_y; y0+=2*Bdelta_y) {
    for (j=0; j<2; j++) {
      
      rAx = Br11*(butterfly_e_z1[j]-Bcz) + Br12*(butterfly_e_x1[j]-Bcx);
      rAz = Br21*(butterfly_e_z1[j]-Bcz) + Br22*(butterfly_e_x1[j]-Bcx);

      rBx = Br11*(butterfly_e_z1[j+1]-Bcz) + Br12*(butterfly_e_x1[j+1]-Bcx);
      rBz = Br21*(butterfly_e_z1[j+1]-Bcz) + Br22*(butterfly_e_x1[j+1]-Bcx);

      mcdis_line(rAx, y0, rAz, rBx, y0, rBz);

      rAx = Br11*(butterfly_e_z2[j]-Bcz) + Br12*(butterfly_e_x2[j]-Bcx);
      rAz = Br21*(butterfly_e_z2[j]-Bcz) + Br22*(butterfly_e_x2[j]-Bcx);

      rBx = Br11*(butterfly_e_z2[j+1]-Bcz) + Br12*(butterfly_e_x2[j+1]-Bcx);
      rBz = Br21*(butterfly_e_z2[j+1]-Bcz) + Br22*(butterfly_e_x2[j+1]-Bcx);

      mcdis_line(rAx, y0, rAz, rBx, y0, rBz);
    }
  }

  /* Indicate the emission planes of cold/thermal moderator */
  for (y0=-Bdelta_y; y0<2*Bdelta_y; y0+=2*Bdelta_y) {
    mcdis_dashed_line(BrC1_x, y0, BrC1_z, BrC2_x, y0, BrC2_z, 11);
    mcdis_dashed_line(BrC1_x, y0, BrC1_z, BrC3_x, y0, BrC3_z, 11);
    mcdis_dashed_line(BrT1_x, y0, BrT1_z, BrT2_x, y0, BrT2_z, 11);
    mcdis_dashed_line(BrT1_x, y0, BrT1_z, BrT3_x, y0, BrT3_z, 11);
  }
  mcdis_dashed_line(BrC1_x, -Bdelta_y, BrC1_z, BrC1_x, Bdelta_y, BrC1_z, 11);
  mcdis_dashed_line(BrC2_x, -Bdelta_y, BrC2_z, BrC2_x, Bdelta_y, BrC2_z, 11);
  mcdis_dashed_line(BrC3_x, -Bdelta_y, BrC3_z, BrC3_x, Bdelta_y, BrC3_z, 11);
  mcdis_dashed_line(BrT1_x, -Bdelta_y, BrT1_z, BrT1_x, Bdelta_y, BrT1_z, 11);
  mcdis_dashed_line(BrT2_x, -Bdelta_y, BrT2_z, BrT2_x, Bdelta_y, BrT2_z, 11);
  mcdis_dashed_line(BrT3_x, -Bdelta_y, BrT3_z, BrT3_x, Bdelta_y, BrT3_z, 11);


  /* Arrow indicating proton beam direction */
  double ax,az,bx,bz,bbx,bbz,cBcx,cBcz;
  az = -0.0925-Bcz;
  ax = 0.0585-Bcx;
  bz = -0.0925-Bcz;
  bx = 0.0585+6-Bcx;
  bbx = 0.0585+0.1-Bcx;
  bbz = -0.0925+0.03-Bcz;
  cBcx = 0.0585+0.1-Bcx;
  cBcz = -0.0925-0.03-Bcz;
  /* rAx,0,rAz is the centre of the moderator */
  rAx = Br11*(az) + Br12*(ax);
  rAz = Br21*(az) + Br22*(ax);
  rBx = Br11*(bz) + Br12*(bx);
  rBz = Br21*(bz) + Br22*(bx);
  /* Main part of the arrow */
  mcdis_line(rAx, 0, rAz, rBx, 0, rBz);
  /* Inclined lines for arrow head */
  rBx = Br11*(bbz) + Br12*(bbx);
  rBz = Br21*(bbz) + Br22*(bbx);
  mcdis_line(rAx, 0, rAz, rBx, 0, rBz);
  rBx = Br11*(cBcz) + Br12*(cBcx);
  rBz = Br21*(cBcz) + Br22*(cBcx);
  mcdis_line(rAx, 0, rAz, rBx, 0, rBz);

  /* 120 degree "end of sector" lines */
  bbz = 2 * cos(DEG2RAD*61);
  bbx = 2 * sin(DEG2RAD*61);
  cBcz = 2 * cos(-DEG2RAD*61);
  cBcx = 2 * sin(-DEG2RAD*61);
  rBx = Br11*(bbz) + Br12*(bbx);
  rBz = Br21*(bbz) + Br22*(bbx);
  mcdis_dashed_line(rAx, 0, rAz, rBx+rAx, 0, rBz+rAz,51);
  rBx = Br11*(cBcz) + Br12*(cBcx);
  rBz = Br21*(cBcz) + Br22*(cBcx);
  mcdis_dashed_line(rAx, 0, rAz, rBx+rAx, 0, rBz+rAz,51);
  bbz = 2 * cos(DEG2RAD*119);
  bbx = 2 * sin(DEG2RAD*119);
  cBcz = 2 * cos(-DEG2RAD*119);
  cBcx = 2 * sin(-DEG2RAD*119);
  rBx = Br11*(bbz) + Br12*(bbx);
  rBz = Br21*(bbz) + Br22*(bbx);
  mcdis_dashed_line(rAx, 0, rAz, rBx+rAx, 0, rBz+rAz,51);
  rBx = Br11*(cBcz) + Br12*(cBcx);
  rBz = Br21*(cBcz) + Br22*(cBcx);
  mcdis_dashed_line(rAx, 0, rAz, rBx+rAx, 0, rBz+rAz,51);
  /* Circles indicating extent of the "empBty" zone where optics is not allowed */
  mcdis_circle("xz", rAx, 0, rAz, 2.0);
  mcdis_circle("xz", rAx, -0.1, rAz, 2.0);
  mcdis_circle("xz", rAx, 0.1, rAz, 2.0);

  /* Circles indicating extent of the target monolith */
  mcdis_circle("xz", rAx, 0, rAz, 5.5);
  mcdis_circle("xz", rAx, -1, rAz, 5.5);
  mcdis_circle("xz", rAx, 1, rAz, 5.5);

  /* Beamport "plug" dimensions */
  double w1=0.206/2.0, w2=0.276/2.0, l1=2.0+rAz, l2=2.0+rAz+1.75, l3=2.0+rAz+3.5;
  mcdis_line(w1, 0, l1, w1, 0, l2);
  mcdis_line(-w1, 0, l1, -w1, 0, l2);
  mcdis_line(w1, 0, l2, w2, 0, l2);
  mcdis_line(-w1, 0, l2, -w2, 0, l2);
  mcdis_line(w2, 0, l2, w2, 0, l3);
  mcdis_line(-w2, 0, l2, -w2, 0, l3);

  /* Draw all the BBeamlines in "this sector" +1 */
  double xx1, yy1, zz1, xx2, yy2, zz2, delta_omega;
  for (j=0; j<Bjmax+1; j++) {
    delta_omega = Borientation_angle - BBeamlines[j];
    Br11 = cos(DEG2RAD*delta_omega);
    Br12 = -sin(DEG2RAD*delta_omega);
    Br21 = sin(DEG2RAD*delta_omega);
    Br22 = cos(DEG2RAD*delta_omega);
    xx1 = Br11*(w1) + Br12*(l1);
    zz1 = Br21*(w1) + Br22*(l1);
    xx2 = Br11*(w1) + Br12*(l2);
    zz2 = Br21*(w1) + Br22*(l2);
    mcdis_dashed_line(xx1, 0, zz1, xx2, 0, zz2, 11);
    xx1 = Br11*(-w1) + Br12*(l1);
    zz1 = Br21*(-w1) + Br22*(l1);
    xx2 = Br11*(-w1) + Br12*(l2);
    zz2 = Br21*(-w1) + Br22*(l2);
    mcdis_dashed_line(xx1, 0, zz1, xx2, 0, zz2, 11);
    xx1 = Br11*(w2) + Br12*(l2);
    zz1 = Br21*(w2) + Br22*(l2);
    xx2 = Br11*(w2) + Br12*(l3);
    zz2 = Br21*(w2) + Br22*(l3);
    mcdis_dashed_line(xx1, 0, zz1, xx2, 0, zz2, 11);
    xx1 = Br11*(-w2) + Br12*(l2);
    zz1 = Br21*(-w2) + Br22*(l2);
    xx2 = Br11*(-w2) + Br12*(l3);
    zz2 = Br21*(-w2) + Br22*(l3);
    mcdis_dashed_line(xx1, 0, zz1, xx2, 0, zz2, 11);
  }

  /* Show instrument axis... */
  mcdis_dashed_line(0,0,0,0,0,2+rAz,21);

  /* Draw up the "focusing rectangle" */ 
  /* Horizontal direction vector @ focusing area */
  vec_prod(xx1,yy1,zz1,Btx,Bty,Btz,0.0,1.0,0.0);
  NORM(xx1,yy1,zz1);
  vec_prod(xx2,yy2,zz2,Btx,Bty,Btz,xx1,yy1,zz1);
  NORM(xx2,yy2,zz2);
  xx1*=Bfoc_XW/2.0; yy1*=Bfoc_XW/2.0; zz1*=Bfoc_XW/2.0;
  xx2*=Bfoc_YH/2.0; yy2*=Bfoc_YH/2.0; zz2*=Bfoc_YH/2.0;
  printf("Normal vectors pointing in directions\n %g %g %g and \n %g %g %g \n",xx1,yy1,zz1,xx2,yy2,zz2);
  mcdis_dashed_line(Btx -xx1 -xx2, Bty -yy1 -yy2, Btz -zz1 -zz2,
	      Btx +xx1 -xx2, Bty +yy1 -yy2, Btz +zz1 -zz2,5);
  mcdis_dashed_line(Btx -xx1 +xx2, Bty -yy1 +yy2, Btz -zz1 +zz2,
	      Btx +xx1 +xx2, Bty +yy1 +yy2, Btz +zz1 +zz2,5);

  mcdis_dashed_line(Btx -xx1 -xx2, Bty -yy1 -yy2, Btz -zz1 -zz2,
	      Btx -xx1 +xx2, Bty -yy1 +yy2, Btz -zz1 +zz2,5);
  mcdis_dashed_line(Btx +xx1 -xx2, Bty +yy1 -yy2, Btz +zz1 -zz2,
	      Btx +xx1 +xx2, Bty +yy1 +yy2, Btz +zz1 +zz2,5);
}
