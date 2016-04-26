/* MCDISPLAY-section for the ESS butterfly moderator */

/* Define the positioning of the buttefly sketch */
double dy = 0.015;

/* Point sets for the butterfly */
double butterfly_z[] = {-1.9922764e-01, -1.8484553e-01, -2.0252845e-01, -2.0795122e-01, -2.1054471e-01, -2.1030894e-01, -2.0889431e-01, -2.0535772e-01, -2.0134959e-01, -1.9639837e-01, -1.9026829e-01, -1.8390244e-01, -1.7565041e-01, -1.7093496e-01, -1.4617886e-01, -1.2873171e-01, -9.2658537e-02, -4.0552845e-02, -1.7682927e-02, -9.1951221e-03, -2.3577231e-03, 5.1869889e-03, 1.1788619e-02, 1.7918699e-02, 2.3105689e-02, 2.4991869e-02, 2.4756099e-02, 2.2162599e-02, 1.8154469e-02, 1.2731709e-02, 5.6585389e-03, -2.3577214e-04, 1.4146339e-02, -2.9707317e-02, 1.4146339e-02, -4.7154514e-04, 1.5325199e-02, 2.1691059e-02, 2.4991869e-02, 2.4991869e-02, 2.2634149e-02, 1.8154469e-02, 1.1552849e-02, 3.3008089e-03, -6.1300811e-03, -9.1951221e-03, -6.0593496e-02, -9.2658537e-02, -1.7541463e-01, -1.8508130e-01, -1.9309756e-01, -1.9899187e-01, -2.0182114e-01, -2.0582927e-01, -2.0913008e-01, -2.1078049e-01, -2.1007317e-01, -2.0630081e-01, -2.0229268e-01, -1.9828455e-01, -1.8484553e-01, -1.9922764e-01, -1.5584553e-01, -1.9922764e-01};

double butterfly_x[] = {1.4279319e-02, 3.1692034e-10, -1.7654432e-02, -2.5962400e-02, -3.4789615e-02, -4.3876452e-02, -5.1665172e-02, -5.8934642e-02, -6.4646372e-02, -6.9059982e-02, -7.2694722e-02, -7.5031332e-02, -7.6589082e-02, -7.6589082e-02, -7.6848702e-02, -7.6589082e-02, -7.6589082e-02, -7.6589082e-02, -7.6589082e-02, -7.6848702e-02, -7.5290962e-02, -7.2694722e-02, -6.8281112e-02, -6.1790512e-02, -5.3482542e-02, -4.4136082e-02, -3.3491495e-02, -2.5443152e-02, -1.9212176e-02, -1.2981200e-02, -6.2309757e-03, -2.5962368e-04, 1.4538943e-02, 5.8415398e-02, 1.0229185e-01, 1.1709042e-01, 1.3266786e-01, 1.4149508e-01, 1.5213966e-01, 1.6200537e-01, 1.7135184e-01, 1.7888093e-01, 1.8589078e-01, 1.9082364e-01, 1.9290063e-01, 1.9341988e-01, 1.9341988e-01, 1.9341988e-01, 1.9341988e-01, 1.9186213e-01, 1.8822740e-01, 1.8459266e-01, 1.8069830e-01, 1.7602507e-01, 1.6849597e-01, 1.5811101e-01, 1.4928380e-01, 1.3993733e-01, 1.3370636e-01, 1.2981200e-01, 1.1709042e-01, 1.0229185e-01, 5.8415398e-02, 1.4279319e-02};

/* Draw the two butterfly shapes at top and bottom level */
magnify("");
double y0;
int j;
double r11, r12, r21, r22;
r11 = cos(DEG2RAD*orientation_angle);
r12 = -sin(DEG2RAD*orientation_angle);
r21 = sin(DEG2RAD*orientation_angle);
r22 = cos(DEG2RAD*orientation_angle);

double rAx,rAz,rBx,rBz;

for (y0=-dy; y0<2*dy; y0+=2*dy) {
  for (j=0; j<63; j++) {
    
    rAx = r11*(butterfly_z[j]-cz) + r12*(butterfly_x[j]-cx);
    rAz = r21*(butterfly_z[j]-cz) + r22*(butterfly_x[j]-cx);

    rBx = r11*(butterfly_z[j+1]-cz) + r12*(butterfly_x[j+1]-cx);
    rBz = r21*(butterfly_z[j+1]-cz) + r22*(butterfly_x[j+1]-cx);

    line(rAx, y0, rAz, rBx, y0, rBz);
    //line(butterfly_x[j]-cx, y0, butterfly_z[j]-cz, butterfly_x[j+1]-cx, y0, butterfly_z[j+1]-cz);

  }
}
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
bbz = 2 * cos(DEG2RAD*60);
bbx = 2 * sin(DEG2RAD*60);
ccz = 2 * cos(-DEG2RAD*60);
ccx = 2 * sin(-DEG2RAD*60);
rBx = r11*(bbz) + r12*(bbx);
rBz = r21*(bbz) + r22*(bbx);
dashed_line(rAx, 0, rAz, rBx, 0, rBz,51);
rBx = r11*(ccz) + r12*(ccx);
rBz = r21*(ccz) + r22*(ccx);
dashed_line(rAx, 0, rAz, rBx, 0, rBz,51);
bbz = 2 * cos(DEG2RAD*120);
bbx = 2 * sin(DEG2RAD*120);
ccz = 2 * cos(-DEG2RAD*120);
ccx = 2 * sin(-DEG2RAD*120);
rBx = r11*(bbz) + r12*(bbx);
rBz = r21*(bbz) + r22*(bbx);
dashed_line(rAx, 0, rAz, rBx, 0, rBz,51);
rBx = r11*(ccz) + r12*(ccx);
rBz = r21*(ccz) + r22*(ccx);
dashed_line(rAx, 0, rAz, rBx, 0, rBz,51);

/* Circles indicating extent of the "empty" zone where optics is not allowed */
circle("xz", rAx, 0, rAz, 2.0);
circle("xz", rAx, -0.1, rAz, 2.0);
circle("xz", rAx, 0.1, rAz, 2.0);
/* Circles indicating extent of the target monolith */
circle("xz", rAx, 0, rAz, 5.5);
circle("xz", rAx, -1, rAz, 5.5);
circle("xz", rAx, 1, rAz, 5.5);
/* Beamport "plug" dimensions FIXME: Put the right dimensions in...*/
dashed_line(0.04, 0, 2+rAz, 0.04, 0, 3+rAz, 11);
dashed_line(-0.04, 0, 2+rAz, -0.04, 0, 3+rAz, 11);
dashed_line(0.08, 0, 3+rAz, 0.08, 0, 5.5+rAz, 31);
dashed_line(-0.08, 0, 3+rAz, -0.08, 0, 5.5+rAz, 31);

/* Show instrument axis... */
dashed_line(0,0,0,0,0,2+rAz,21);
