double form_volume(void);

double Iq(double q,
    double lg_radius, double sm_radius,
    double lg_vol_frac, double sm_vol_frac,
    double lg_sld, double sm_sld, double solvent_sld
    );

void calculate_psfs(double qval,
    double r2, double nf2,
    double aa, double phi,
    double *s11, double *s22, double *s12
    );

double form_volume(void)
{
    return 1.0;
}

double Iq(double q,
    double lg_radius, double sm_radius,
    double lg_vol_frac, double sm_vol_frac,
    double lg_sld, double sm_sld, double solvent_sld)
{
    double r2,r1,nf2,phi,aa,rho2,rho1,rhos,inten;       //my local names
    double psf11,psf12,psf22;
    double phi1,phi2,phr,a3;
    double v1,v2,n1,n2,qr1,qr2,b1,b2,sc1,sc2;

    r2 = lg_radius;
    r1 = sm_radius;
    phi2 = lg_vol_frac;
    phi1 = sm_vol_frac;
    rho2 = lg_sld;
    rho1 = sm_sld;
    rhos = solvent_sld;


    phi = phi1 + phi2;
    aa = r1/r2;
    //calculate the number fraction of larger spheres (eqn 2 in reference)
    a3=aa*aa*aa;
    phr=phi2/phi;
    nf2 = phr*a3/(1.0-phr+phr*a3);
    // calculate the PSF's here
    calculate_psfs(q,r2,nf2,aa,phi,&psf11,&psf22,&psf12);

    // /* do form factor calculations  */

    v1 = M_4PI_3*r1*r1*r1;
    v2 = M_4PI_3*r2*r2*r2;

    n1 = phi1/v1;
    n2 = phi2/v2;

    qr1 = r1*q;
    qr2 = r2*q;

    sc1 = sas_3j1x_x(qr1);
    sc2 = sas_3j1x_x(qr2);
    b1 = r1*r1*r1*(rho1-rhos)*M_4PI_3*sc1;
    b2 = r2*r2*r2*(rho2-rhos)*M_4PI_3*sc2;
    inten = n1*b1*b1*psf11;
    inten += sqrt(n1*n2)*2.0*b1*b2*psf12;
    inten += n2*b2*b2*psf22;
    ///* convert I(1/A) to (1/cm)  */
    inten *= 1.0e8;
    ///*convert rho^2 in 10^-6A to A*/
    inten *= 1.0e-12;
    return(inten);
}


void calculate_psfs(double qval,
    double r2, double nf2,
    double aa, double phi,
    double *s11, double *s22, double *s12)
{
    //  variable qval,r2,nf2,aa,phi,&s11,&s22,&s12

    //   calculate constant terms
    double s2,v,a3,v1,v2,g11,g12,g22,wmv,wmv3,wmv4;
    double a1,a2i,a2,b1,b2,b12,gm1,gm12;
    double yy,ay,ay2,ay3,t1,t2,t3,f11,y2,y3,tt1,tt2,tt3;
    double c11,c22,c12,f12,f22,ttt1,ttt2,ttt3,ttt4,yl,y13;
    double t21,t22,t23,t31,t32,t33,t41,t42,yl3,wma3,y1;

    s2 = 2.0*r2;
//    s1 = aa*s2;  why is this never used?  check original paper?
    v = phi;
    a3 = aa*aa*aa;
    v1=((1.-nf2)*a3/(nf2+(1.-nf2)*a3))*v;
    v2=(nf2/(nf2+(1.-nf2)*a3))*v;
    g11=((1.+.5*v)+1.5*v2*(aa-1.))/(1.-v)/(1.-v);
    g22=((1.+.5*v)+1.5*v1*(1./aa-1.))/(1.-v)/(1.-v);
    g12=((1.+.5*v)+1.5*(1.-aa)*(v1-v2)/(1.+aa))/(1.-v)/(1.-v);
    wmv = 1/(1.-v);
    wmv3 = wmv*wmv*wmv;
    wmv4 = wmv*wmv3;
    a1=3.*wmv4*((v1+a3*v2)*(1.+v+v*v)-3.*v1*v2*(1.-aa)*(1.-aa)*(1.+v1+aa*(1.+v2))) + ((v1+a3*v2)*(1.+2.*v)+(1.+v+v*v)-3.*v1*v2*(1.-aa)*(1.-aa)-3.*v2*(1.-aa)*(1.-aa)*(1.+v1+aa*(1.+v2)))*wmv3;
    a2i=((v1+a3*v2)*(1.+v+v*v)-3.*v1*v2*(1.-aa)*(1.-aa)*(1.+v1+aa*(1.+v2)))*3*wmv4 + ((v1+a3*v2)*(1.+2.*v)+a3*(1.+v+v*v)-3.*v1*v2*(1.-aa)*(1.-aa)*aa-3.*v1*(1.-aa)*(1.-aa)*(1.+v1+aa*(1.+v2)))*wmv3;
    a2=a2i/a3;
    b1=-6.*(v1*g11*g11+.25*v2*(1.+aa)*(1.+aa)*aa*g12*g12);
    b2=-6.*(v2*g22*g22+.25*v1/a3*(1.+aa)*(1.+aa)*g12*g12);
    b12=-3.*aa*(1.+aa)*(v1*g11/aa/aa+v2*g22)*g12;
    gm1=(v1*a1+a3*v2*a2)*.5;
    gm12=2.*gm1*(1.-aa)/aa;
    //c
    //c   calculate the direct correlation functions and print results
    //c
    //  do 20 j=1,npts

    yy=qval*s2;
    //c   calculate direct correlation functions
    //c   ----c11
    ay=aa*yy;
    ay2 = ay*ay;
    ay3 = ay*ay*ay;
    t1=a1*(sin(ay)-ay*cos(ay));
    t2=b1*(2.*ay*sin(ay)-(ay2-2.)*cos(ay)-2.)/ay;
    t3=gm1*((4.*ay*ay2-24.*ay)*sin(ay)-(ay2*ay2-12.*ay2+24.)*cos(ay)+24.)/ay3;
    f11=24.*v1*(t1+t2+t3)/ay3;

    //c ------c22
    y2=yy*yy;
    y3=yy*y2;
    tt1=a2*(sin(yy)-yy*cos(yy));
    tt2=b2*(2.*yy*sin(yy)-(y2-2.)*cos(yy)-2.)/yy;
    tt3=gm1*((4.*y3-24.*yy)*sin(yy)-(y2*y2-12.*y2+24.)*cos(yy)+24.)/ay3;
    f22=24.*v2*(tt1+tt2+tt3)/y3;

    //c   -----c12
    yl=.5*yy*(1.-aa);
    yl3=yl*yl*yl;
    wma3 = (1.-aa)*(1.-aa)*(1.-aa);
    y1=aa*yy;
    y13 = y1*y1*y1;
    ttt1=3.*wma3*v*sqrt(nf2)*sqrt(1.-nf2)*a1*(sin(yl)-yl*cos(yl))/((nf2+(1.-nf2)*a3)*yl3);
    t21=b12*(2.*y1*cos(y1)+(y1*y1-2.)*sin(y1));
    t22=gm12*((3.*y1*y1-6.)*cos(y1)+(y1*y1*y1-6.*y1)*sin(y1)+6.)/y1;
    t23=gm1*((4.*y13-24.*y1)*cos(y1)+(y13*y1-12.*y1*y1+24.)*sin(y1))/(y1*y1);
    t31=b12*(2.*y1*sin(y1)-(y1*y1-2.)*cos(y1)-2.);
    t32=gm12*((3.*y1*y1-6.)*sin(y1)-(y1*y1*y1-6.*y1)*cos(y1))/y1;
    t33=gm1*((4.*y13-24.*y1)*sin(y1)-(y13*y1-12.*y1*y1+24.)*cos(y1)+24.)/(y1*y1);
    t41=cos(yl)*((sin(y1)-y1*cos(y1))/(y1*y1) + (1.-aa)/(2.*aa)*(1.-cos(y1))/y1);
    t42=sin(yl)*((cos(y1)+y1*sin(y1)-1.)/(y1*y1) + (1.-aa)/(2.*aa)*sin(y1)/y1);
    ttt2=sin(yl)*(t21+t22+t23)/(y13*y1);
    ttt3=cos(yl)*(t31+t32+t33)/(y13*y1);
    ttt4=a1*(t41+t42)/y1;
    f12=ttt1+24.*v*sqrt(nf2)*sqrt(1.-nf2)*a3*(ttt2+ttt3+ttt4)/(nf2+(1.-nf2)*a3);

    c11=f11;
    c22=f22;
    c12=f12;
    *s11=1./(1.+c11-(c12)*c12/(1.+c22));
    *s22=1./(1.+c22-(c12)*c12/(1.+c11));
    *s12=-c12/((1.+c11)*(1.+c22)-(c12)*(c12));

    return;
}
