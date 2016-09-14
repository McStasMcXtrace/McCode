/* MCDISPLAY-section for the ESS butterfly moderator */

/* Define the positioning of the buttefly sketch */

/* Point sets for the butterfly */
double butterfly_z[] = {-1.9922764e-01, -1.8484553e-01, -2.0252845e-01, -2.0795122e-01, -2.1054471e-01, -2.1030894e-01, -2.0889431e-01, -2.0535772e-01, -2.0134959e-01, -1.9639837e-01, -1.9026829e-01, -1.8390244e-01, -1.7565041e-01, -1.7093496e-01, -1.4617886e-01, -1.2873171e-01, -9.2658537e-02, -4.0552845e-02, -1.7682927e-02, -9.1951221e-03, -2.3577231e-03, 5.1869889e-03, 1.1788619e-02, 1.7918699e-02, 2.3105689e-02, 2.4991869e-02, 2.4756099e-02, 2.2162599e-02, 1.8154469e-02, 1.2731709e-02, 5.6585389e-03, -2.3577214e-04, 1.4146339e-02, -2.9707317e-02, 1.4146339e-02, -4.7154514e-04, 1.5325199e-02, 2.1691059e-02, 2.4991869e-02, 2.4991869e-02, 2.2634149e-02, 1.8154469e-02, 1.1552849e-02, 3.3008089e-03, -6.1300811e-03, -9.1951221e-03, -6.0593496e-02, -9.2658537e-02, -1.7541463e-01, -1.8508130e-01, -1.9309756e-01, -1.9899187e-01, -2.0182114e-01, -2.0582927e-01, -2.0913008e-01, -2.1078049e-01, -2.1007317e-01, -2.0630081e-01, -2.0229268e-01, -1.9828455e-01, -1.8484553e-01, -1.9922764e-01, -1.5584553e-01, -1.9922764e-01};

double butterfly_x[] = {1.4279319e-02, 3.1692034e-10, -1.7654432e-02, -2.5962400e-02, -3.4789615e-02, -4.3876452e-02, -5.1665172e-02, -5.8934642e-02, -6.4646372e-02, -6.9059982e-02, -7.2694722e-02, -7.5031332e-02, -7.6589082e-02, -7.6589082e-02, -7.6848702e-02, -7.6589082e-02, -7.6589082e-02, -7.6589082e-02, -7.6589082e-02, -7.6848702e-02, -7.5290962e-02, -7.2694722e-02, -6.8281112e-02, -6.1790512e-02, -5.3482542e-02, -4.4136082e-02, -3.3491495e-02, -2.5443152e-02, -1.9212176e-02, -1.2981200e-02, -6.2309757e-03, -2.5962368e-04, 1.4538943e-02, 5.8415398e-02, 1.0229185e-01, 1.1709042e-01, 1.3266786e-01, 1.4149508e-01, 1.5213966e-01, 1.6200537e-01, 1.7135184e-01, 1.7888093e-01, 1.8589078e-01, 1.9082364e-01, 1.9290063e-01, 1.9341988e-01, 1.9341988e-01, 1.9341988e-01, 1.9341988e-01, 1.9186213e-01, 1.8822740e-01, 1.8459266e-01, 1.8069830e-01, 1.7602507e-01, 1.6849597e-01, 1.5811101e-01, 1.4928380e-01, 1.3993733e-01, 1.3370636e-01, 1.2981200e-01, 1.1709042e-01, 1.0229185e-01, 5.8415398e-02, 1.4279319e-02};

double butterfly_e_z1[]= {-3.0488e-04,  -5.5017e-02, -3.0488e-04};
double butterfly_e_x1[]= {-4.3103e-04,   5.8521e-02,  1.1701e-01};
double butterfly_e_z2[]= {-1.8501e-01, -1.2719e-01, -1.8501e-01};
double butterfly_e_x2[]= {3.3156e-05,  5.8985e-02,  1.1701e-01};
   


/* Draw the two butterfly shapes at top and bottom level */
magnify("");
double y0;
int j;
double rAx,rAz,rBx,rBz;

for (y0=-delta_y; y0<2*delta_y; y0+=2*delta_y) {
  for (j=0; j<63; j++) {
    
    rAx = r11*(butterfly_z[j]-cz) + r12*(butterfly_x[j]-cx);
    rAz = r21*(butterfly_z[j]-cz) + r22*(butterfly_x[j]-cx);

    rBx = r11*(butterfly_z[j+1]-cz) + r12*(butterfly_x[j+1]-cx);
    rBz = r21*(butterfly_z[j+1]-cz) + r22*(butterfly_x[j+1]-cx);

    line(rAx, y0, rAz, rBx, y0, rBz);

  }
}

/* Draw the "border" between the thermal and cold areas */
for (y0=-delta_y; y0<2*delta_y; y0+=2*delta_y) {
  for (j=0; j<2; j++) {
    
    rAx = r11*(butterfly_e_z1[j]-cz) + r12*(butterfly_e_x1[j]-cx);
    rAz = r21*(butterfly_e_z1[j]-cz) + r22*(butterfly_e_x1[j]-cx);

    rBx = r11*(butterfly_e_z1[j+1]-cz) + r12*(butterfly_e_x1[j+1]-cx);
    rBz = r21*(butterfly_e_z1[j+1]-cz) + r22*(butterfly_e_x1[j+1]-cx);

    line(rAx, y0, rAz, rBx, y0, rBz);

    rAx = r11*(butterfly_e_z2[j]-cz) + r12*(butterfly_e_x2[j]-cx);
    rAz = r21*(butterfly_e_z2[j]-cz) + r22*(butterfly_e_x2[j]-cx);

    rBx = r11*(butterfly_e_z2[j+1]-cz) + r12*(butterfly_e_x2[j+1]-cx);
    rBz = r21*(butterfly_e_z2[j+1]-cz) + r22*(butterfly_e_x2[j+1]-cx);

    line(rAx, y0, rAz, rBx, y0, rBz);
  }
}

/* Indicate the emission planes of cold/thermal moderator */
for (y0=-delta_y; y0<2*delta_y; y0+=2*delta_y) {
  dashed_line(rC1_x, y0, rC1_z, rC2_x, y0, rC2_z, 11);
  dashed_line(rC1_x, y0, rC1_z, rC3_x, y0, rC3_z, 11);
  dashed_line(rT1_x, y0, rT1_z, rT2_x, y0, rT2_z, 11);
  dashed_line(rT1_x, y0, rT1_z, rT3_x, y0, rT3_z, 11);
}
dashed_line(rC1_x, -delta_y, rC1_z, rC1_x, delta_y, rC1_z, 11);
dashed_line(rC2_x, -delta_y, rC2_z, rC2_x, delta_y, rC2_z, 11);
dashed_line(rC3_x, -delta_y, rC3_z, rC3_x, delta_y, rC3_z, 11);
dashed_line(rT1_x, -delta_y, rT1_z, rT1_x, delta_y, rT1_z, 11);
dashed_line(rT2_x, -delta_y, rT2_z, rT2_x, delta_y, rT2_z, 11);
dashed_line(rT3_x, -delta_y, rT3_z, rT3_x, delta_y, rT3_z, 11);


/* Arrow indicating proton beam direction */
double ax,az,bx,bz,bbx,bbz,ccx,ccz;
az = -0.0925-cz;
ax = 0.0585-cx;
bz = -0.0925-cz;
bx = 0.0585+6-cx;
bbx = 0.0585+0.1-cx;
bbz = -0.0925+0.03-cz;
ccx = 0.0585+0.1-cx;
ccz = -0.0925-0.03-cz;
/* rAx,0,rAz is the centre of the moderator */
rAx = r11*(az) + r12*(ax);
rAz = r21*(az) + r22*(ax);
rBx = r11*(bz) + r12*(bx);
rBz = r21*(bz) + r22*(bx);
/* Main part of the arrow */
line(rAx, 0, rAz, rBx, 0, rBz);
/* Inclined lines for arrow head */
rBx = r11*(bbz) + r12*(bbx);
rBz = r21*(bbz) + r22*(bbx);
line(rAx, 0, rAz, rBx, 0, rBz);
rBx = r11*(ccz) + r12*(ccx);
rBz = r21*(ccz) + r22*(ccx);
line(rAx, 0, rAz, rBx, 0, rBz);

/* 120 degree "end of sector" lines */
bbz = 2 * cos(DEG2RAD*61);
bbx = 2 * sin(DEG2RAD*61);
ccz = 2 * cos(-DEG2RAD*61);
ccx = 2 * sin(-DEG2RAD*61);
rBx = r11*(bbz) + r12*(bbx);
rBz = r21*(bbz) + r22*(bbx);
dashed_line(rAx, 0, rAz, rBx+rAx, 0, rBz+rAz,51);
rBx = r11*(ccz) + r12*(ccx);
rBz = r21*(ccz) + r22*(ccx);
dashed_line(rAx, 0, rAz, rBx+rAx, 0, rBz+rAz,51);
bbz = 2 * cos(DEG2RAD*119);
bbx = 2 * sin(DEG2RAD*119);
ccz = 2 * cos(-DEG2RAD*119);
ccx = 2 * sin(-DEG2RAD*119);
rBx = r11*(bbz) + r12*(bbx);
rBz = r21*(bbz) + r22*(bbx);
dashed_line(rAx, 0, rAz, rBx+rAx, 0, rBz+rAz,51);
rBx = r11*(ccz) + r12*(ccx);
rBz = r21*(ccz) + r22*(ccx);
dashed_line(rAx, 0, rAz, rBx+rAx, 0, rBz+rAz,51);
/* Circles indicating extent of the "empty" zone where optics is not allowed */
circle("xz", rAx, 0, rAz, 2.0);
circle("xz", rAx, -0.1, rAz, 2.0);
circle("xz", rAx, 0.1, rAz, 2.0);

/* Circles indicating extent of the target monolith */
circle("xz", rAx, 0, rAz, 5.5);
circle("xz", rAx, -1, rAz, 5.5);
circle("xz", rAx, 1, rAz, 5.5);

/* Beamport "plug" dimensions */
double w1=0.206/2.0, w2=0.276/2.0, l1=2.0+rAz, l2=2.0+rAz+1.75, l3=2.0+rAz+3.5;
line(w1, 0, l1, w1, 0, l2);
line(-w1, 0, l1, -w1, 0, l2);
line(w1, 0, l2, w2, 0, l2);
line(-w1, 0, l2, -w2, 0, l2);
line(w2, 0, l2, w2, 0, l3);
line(-w2, 0, l2, -w2, 0, l3);

/* Draw all the beamlines in "this sector" +1 */
double xx1, yy1, zz1, xx2, yy2, zz2, delta_omega;
for (j=0; j<jmax+1; j++) {
  delta_omega = orientation_angle - Beamlines[j];
  r11 = cos(DEG2RAD*delta_omega);
  r12 = -sin(DEG2RAD*delta_omega);
  r21 = sin(DEG2RAD*delta_omega);
  r22 = cos(DEG2RAD*delta_omega);
  xx1 = r11*(w1) + r12*(l1);
  zz1 = r21*(w1) + r22*(l1);
  xx2 = r11*(w1) + r12*(l2);
  zz2 = r21*(w1) + r22*(l2);
  dashed_line(xx1, 0, zz1, xx2, 0, zz2, 11);
  xx1 = r11*(-w1) + r12*(l1);
  zz1 = r21*(-w1) + r22*(l1);
  xx2 = r11*(-w1) + r12*(l2);
  zz2 = r21*(-w1) + r22*(l2);
  dashed_line(xx1, 0, zz1, xx2, 0, zz2, 11);
  xx1 = r11*(w2) + r12*(l2);
  zz1 = r21*(w2) + r22*(l2);
  xx2 = r11*(w2) + r12*(l3);
  zz2 = r21*(w2) + r22*(l3);
  dashed_line(xx1, 0, zz1, xx2, 0, zz2, 11);
  xx1 = r11*(-w2) + r12*(l2);
  zz1 = r21*(-w2) + r22*(l2);
  xx2 = r11*(-w2) + r12*(l3);
  zz2 = r21*(-w2) + r22*(l3);
  dashed_line(xx1, 0, zz1, xx2, 0, zz2, 11);
}

/* Show instrument axis... */
dashed_line(0,0,0,0,0,2+rAz,21);

/* Draw up the "focusing rectangle" */ 
/* Horizontal direction vector @ focusing area */
vec_prod(xx1,yy1,zz1,tx,ty,tz,0.0,1.0,0.0);
NORM(xx1,yy1,zz1);
vec_prod(xx2,yy2,zz2,tx,ty,tz,xx1,yy1,zz1);
NORM(xx2,yy2,zz2);
xx1*=focus_xw/2.0; yy1*=focus_xw/2.0; zz1*=focus_xw/2.0;
xx2*=focus_yh/2.0; yy2*=focus_yh/2.0; zz2*=focus_yh/2.0;
printf("Normal vectors pointing in directions\n %g %g %g and \n %g %g %g \n",xx1,yy1,zz1,xx2,yy2,zz2);
dashed_line(tx -xx1 -xx2, ty -yy1 -yy2, tz -zz1 -zz2,
	    tx +xx1 -xx2, ty +yy1 -yy2, tz +zz1 -zz2,5);
dashed_line(tx -xx1 +xx2, ty -yy1 +yy2, tz -zz1 +zz2,
	    tx +xx1 +xx2, ty +yy1 +yy2, tz +zz1 +zz2,5);

dashed_line(tx -xx1 -xx2, ty -yy1 -yy2, tz -zz1 -zz2,
	    tx -xx1 +xx2, ty -yy1 +yy2, tz -zz1 +zz2,5);
dashed_line(tx +xx1 -xx2, ty +yy1 -yy2, tz +zz1 -zz2,
	    tx +xx1 +xx2, ty +yy1 +yy2, tz +zz1 +zz2,5);
