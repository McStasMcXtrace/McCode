double Iq(double q, double fp_case_num,
    double N[], double Phi[], double v[], double L[], double b[],
    double Kab, double Kac, double Kad,
    double Kbc, double Kbd, double Kcd
    );

double Iq(double q, double fp_case_num,
    double N[],    // DEGREE OF POLYMERIZATION
    double Phi[],  // VOL FRACTION
    double v[],    // SPECIFIC VOLUME
    double L[],    // SCATT. LENGTH
    double b[],    // SEGMENT LENGTH
    double Kab, double Kac, double Kad,  // CHI PARAM
    double Kbc, double Kbd, double Kcd
    )
{
  int icase = (int)(fp_case_num+0.5);

  double Nab,Nac,Nad,Nbc,Nbd,Ncd;
  double Phiab,Phiac,Phiad,Phibc,Phibd,Phicd;
  double vab,vac,vad,vbc,vbd,vcd;
  double m;
  double Xa,Xb,Xc,Xd;
  double Paa,S0aa,Pab,S0ab,Pac,S0ac,Pad,S0ad;
  double S0ba,Pbb,S0bb,Pbc,S0bc,Pbd,S0bd;
  double S0ca,S0cb,Pcc,S0cc,Pcd,S0cd;
  //double S0da,S0db,S0dc;
  double Pdd,S0dd;
  double Kaa,Kbb,Kcc;
  double Kba,Kca,Kcb;
  //double Kda,Kdb,Kdc,Kdd;
  double Zaa,Zab,Zac,Zba,Zbb,Zbc,Zca,Zcb,Zcc;
  double DenT,T11,T12,T13,T21,T22,T23,T31,T32,T33;
  double Y1,Y2,Y3,X11,X12,X13,X21,X22,X23,X31,X32,X33;
  double ZZ,DenQ1,DenQ2,DenQ3,DenQ,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,Q33;
  double N11,N12,N13,N21,N22,N23,N31,N32,N33;
  double M11,M12,M13,M21,M22,M23,M31,M32,M33;
  double S11,S12,S22,S23,S13,S33;
  //double S21,S31,S32,S44; 
  //double S14,S24,S34,S41,S42,S43;
  double Lad,Lbd,Lcd,Nav,Intg;

  // Set values for non existent parameters (eg. no A or B in case 0 and 1 etc)
  //icase was shifted to N-1 from the original code
  if (icase <= 1){
    Phi[0] = Phi[1] = 0.0000001;
    N[0] = N[1] = 1000.0;
    L[0] = L[1] = 1.e-12;
    v[0] = v[1] = 100.0;
    b[0] = b[1] = 5.0;
    Kab = Kac = Kad = Kbc = Kbd = -0.0004;
  }
  else if ((icase > 1) && (icase <= 4)){
    Phi[0] = 0.0000001;
    N[0] = 1000.0;
    L[0] = 1.e-12;
    v[0] = 100.0;
    b[0] = 5.0;
    Kab = Kac = Kad = -0.0004;
  }

  // Set volume fraction of component D based on constraint that sum of vol frac =1
  Phi[3]=1.0-Phi[0]-Phi[1]-Phi[2];

  //set up values for cross terms in case of block copolymers (1,3,4,6,7,8,9)
  Nab=sqrt(N[0]*N[1]);
  Nac=sqrt(N[0]*N[2]);
  Nad=sqrt(N[0]*N[3]);
  Nbc=sqrt(N[1]*N[2]);
  Nbd=sqrt(N[1]*N[3]);
  Ncd=sqrt(N[2]*N[3]);

  vab=sqrt(v[0]*v[1]);
  vac=sqrt(v[0]*v[2]);
  vad=sqrt(v[0]*v[3]);
  vbc=sqrt(v[1]*v[2]);
  vbd=sqrt(v[1]*v[3]);
  vcd=sqrt(v[2]*v[3]);

  Phiab=sqrt(Phi[0]*Phi[1]);
  Phiac=sqrt(Phi[0]*Phi[2]);
  Phiad=sqrt(Phi[0]*Phi[3]);
  Phibc=sqrt(Phi[1]*Phi[2]);
  Phibd=sqrt(Phi[1]*Phi[3]);
  Phicd=sqrt(Phi[2]*Phi[3]);

  // Calculate Q^2 * Rg^2 for each homopolymer assuming random walk
  Xa=q*q*b[0]*b[0]*N[0]/6.0;
  Xb=q*q*b[1]*b[1]*N[1]/6.0;
  Xc=q*q*b[2]*b[2]*N[2]/6.0;
  Xd=q*q*b[3]*b[3]*N[3]/6.0;

  //calculate all partial structure factors Pij and normalize n^2
  Paa=2.0*(exp(-Xa)-1.0+Xa)/(Xa*Xa); // free A chain form factor
  S0aa=N[0]*Phi[0]*v[0]*Paa; // Phi * Vp * P(Q)= I(Q0)/delRho^2
  Pab=((1.0-exp(-Xa))/Xa)*((1.0-exp(-Xb))/Xb); //AB diblock (anchored Paa * anchored Pbb) partial form factor
  S0ab=(Phiab*vab*Nab)*Pab;
  Pac=((1.0-exp(-Xa))/Xa)*exp(-Xb)*((1.0-exp(-Xc))/Xc); //ABC triblock AC partial form factor
  S0ac=(Phiac*vac*Nac)*Pac;
  Pad=((1.0-exp(-Xa))/Xa)*exp(-Xb-Xc)*((1.0-exp(-Xd))/Xd); //ABCD four block
  S0ad=(Phiad*vad*Nad)*Pad;

  S0ba=S0ab;
  Pbb=2.0*(exp(-Xb)-1.0+Xb)/(Xb*Xb); // free B chain
  S0bb=N[1]*Phi[1]*v[1]*Pbb;
  Pbc=((1.0-exp(-Xb))/Xb)*((1.0-exp(-Xc))/Xc); // BC diblock
  S0bc=(Phibc*vbc*Nbc)*Pbc;
  Pbd=((1.0-exp(-Xb))/Xb)*exp(-Xc)*((1.0-exp(-Xd))/Xd); // BCD triblock
  S0bd=(Phibd*vbd*Nbd)*Pbd;

  S0ca=S0ac;
  S0cb=S0bc;
  Pcc=2.0*(exp(-Xc)-1.0+Xc)/(Xc*Xc); // Free C chain
  S0cc=N[2]*Phi[2]*v[2]*Pcc;
  Pcd=((1.0-exp(-Xc))/Xc)*((1.0-exp(-Xd))/Xd); // CD diblock
  S0cd=(Phicd*vcd*Ncd)*Pcd;

  //S0da=S0ad;
  //S0db=S0bd;
  //S0dc=S0cd;
  Pdd=2.0*(exp(-Xd)-1.0+Xd)/(Xd*Xd); // free D chain
  S0dd=N[3]*Phi[3]*v[3]*Pdd;

  // Reset all unused partial structure factors to 0 (depends on case)
  //icase was shifted to N-1 from the original code
  switch(icase){
  case 0:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    S0bb=0.000005;
    S0bc=0.000006;
    S0bd=0.000007;
    S0cd=0.000008;
    break;
  case 1:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    S0bb=0.000005;
    S0bc=0.000006;
    S0bd=0.000007;
    break;
  case 2:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    S0bc=0.000005;
    S0bd=0.000006;
    S0cd=0.000007;
    break;
  case 3:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    S0bc=0.000005;
    S0bd=0.000006;
    break;
  case 4:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    break;
  case 5:
    S0ab=0.000001;
    S0ac=0.000002;
    S0ad=0.000003;
    S0bc=0.000004;
    S0bd=0.000005;
    S0cd=0.000006;
    break;
  case 6:
    S0ab=0.000001;
    S0ac=0.000002;
    S0ad=0.000003;
    S0bc=0.000004;
    S0bd=0.000005;
    break;
  case 7:
    S0ab=0.000001;
    S0ac=0.000002;
    S0ad=0.000003;
    break;
  case 8:
    S0ac=0.000001;
    S0ad=0.000002;
    S0bc=0.000003;
    S0bd=0.000004;
    break;
  default : //case 9:
    break;
  }
  S0ba=S0ab;
  S0ca=S0ac;
  S0cb=S0bc;
  //S0da=S0ad;
  //S0db=S0bd;
  //S0dc=S0cd;

  // self chi parameter is 0 ... of course
  Kaa=0.0;
  Kbb=0.0;
  Kcc=0.0;
  //Kdd=0.0;

  Kba=Kab;
  Kca=Kac;
  Kcb=Kbc;
  //Kda=Kad;
  //Kdb=Kbd;
  //Kdc=Kcd;

  Zaa=Kaa-Kad-Kad;
  Zab=Kab-Kad-Kbd;
  Zac=Kac-Kad-Kcd;
  Zba=Kba-Kbd-Kad;
  Zbb=Kbb-Kbd-Kbd;
  Zbc=Kbc-Kbd-Kcd;
  Zca=Kca-Kcd-Kad;
  Zcb=Kcb-Kcd-Kbd;
  Zcc=Kcc-Kcd-Kcd;

  DenT=(-(S0ac*S0bb*S0ca) + S0ab*S0bc*S0ca + S0ac*S0ba*S0cb - S0aa*S0bc*S0cb - S0ab*S0ba*S0cc + S0aa*S0bb*S0cc);

  T11= (-(S0bc*S0cb) + S0bb*S0cc)/DenT;
  T12= (S0ac*S0cb - S0ab*S0cc)/DenT;
  T13= (-(S0ac*S0bb) + S0ab*S0bc)/DenT;
  T21= (S0bc*S0ca - S0ba*S0cc)/DenT;
  T22= (-(S0ac*S0ca) + S0aa*S0cc)/DenT;
  T23= (S0ac*S0ba - S0aa*S0bc)/DenT;
  T31= (-(S0bb*S0ca) + S0ba*S0cb)/DenT;
  T32= (S0ab*S0ca - S0aa*S0cb)/DenT;
  T33= (-(S0ab*S0ba) + S0aa*S0bb)/DenT;

  Y1=T11*S0ad+T12*S0bd+T13*S0cd+1.0;
  Y2=T21*S0ad+T22*S0bd+T23*S0cd+1.0;
  Y3=T31*S0ad+T32*S0bd+T33*S0cd+1.0;

  X11=Y1*Y1;
  X12=Y1*Y2;
  X13=Y1*Y3;
  X21=Y2*Y1;
  X22=Y2*Y2;
  X23=Y2*Y3;
  X31=Y3*Y1;
  X32=Y3*Y2;
  X33=Y3*Y3;

  ZZ=S0ad*(T11*S0ad+T12*S0bd+T13*S0cd)+S0bd*(T21*S0ad+T22*S0bd+T23*S0cd)+S0cd*(T31*S0ad+T32*S0bd+T33*S0cd);

  // D is considered the matrix or background component so enters here
  m=1.0/(S0dd-ZZ);

  N11=m*X11+Zaa;
  N12=m*X12+Zab;
  N13=m*X13+Zac;
  N21=m*X21+Zba;
  N22=m*X22+Zbb;
  N23=m*X23+Zbc;
  N31=m*X31+Zca;
  N32=m*X32+Zcb;
  N33=m*X33+Zcc;

  M11= N11*S0aa + N12*S0ab + N13*S0ac;
  M12= N11*S0ab + N12*S0bb + N13*S0bc;
  M13= N11*S0ac + N12*S0bc + N13*S0cc;
  M21= N21*S0aa + N22*S0ab + N23*S0ac;
  M22= N21*S0ab + N22*S0bb + N23*S0bc;
  M23= N21*S0ac + N22*S0bc + N23*S0cc;
  M31= N31*S0aa + N32*S0ab + N33*S0ac;
  M32= N31*S0ab + N32*S0bb + N33*S0bc;
  M33= N31*S0ac + N32*S0bc + N33*S0cc;

  DenQ1=1.0+M11-M12*M21+M22+M11*M22-M13*M31-M13*M22*M31;
  DenQ2=  M12*M23*M31+M13*M21*M32-M23*M32-M11*M23*M32+M33+M11*M33;
  DenQ3=  -M12*M21*M33+M22*M33+M11*M22*M33;
  DenQ=DenQ1+DenQ2+DenQ3;

  Q11= (1.0 + M22-M23*M32 + M33 + M22*M33)/DenQ;
  Q12= (-M12 + M13*M32 - M12*M33)/DenQ;
  Q13= (-M13 - M13*M22 + M12*M23)/DenQ;
  Q21= (-M21 + M23*M31 - M21*M33)/DenQ;
  Q22= (1.0 + M11 - M13*M31 + M33 + M11*M33)/DenQ;
  Q23= (M13*M21 - M23 - M11*M23)/DenQ;
  Q31= (-M31 - M22*M31 + M21*M32)/DenQ;
  Q32= (M12*M31 - M32 - M11*M32)/DenQ;
  Q33= (1.0 + M11 - M12*M21 + M22 + M11*M22)/DenQ;

  S11= Q11*S0aa + Q21*S0ab + Q31*S0ac;
  S12= Q12*S0aa + Q22*S0ab + Q32*S0ac;
  S13= Q13*S0aa + Q23*S0ab + Q33*S0ac;
  S22= Q12*S0ba + Q22*S0bb + Q32*S0bc;
  S23= Q13*S0ba + Q23*S0bb + Q33*S0bc;
  S33= Q13*S0ca + Q23*S0cb + Q33*S0cc;
  //S21= Q11*S0ba + Q21*S0bb + Q31*S0bc;
  //S31= Q11*S0ca + Q21*S0cb + Q31*S0cc;
  //S32= Q12*S0ca + Q22*S0cb + Q32*S0cc;
  //S44=S11+S22+S33+2.0*S12+2.0*S13+2.0*S23;
  //S14=-S11-S12-S13;
  //S24=-S21-S22-S23;
  //S34=-S31-S32-S33;
  //S41=S14;
  //S42=S24;
  //S43=S34;

  //calculate contrast where L[i] is the scattering length of i and D is the matrix
  //Note that should multiply by Nav to get units of SLD which will become
  // Nav*2 in the next line (SLD^2) but then normalization in that line would
  //need to divide by Nav leaving only Nav or sqrt(Nav) before squaring.
  Nav=6.022045e+23;
  Lad=(L[0]/v[0]-L[3]/v[3])*sqrt(Nav);
  Lbd=(L[1]/v[1]-L[3]/v[3])*sqrt(Nav);
  Lcd=(L[2]/v[2]-L[3]/v[3])*sqrt(Nav);

  Intg=Lad*Lad*S11+Lbd*Lbd*S22+Lcd*Lcd*S33+2.0*Lad*Lbd*S12+2.0*Lbd*Lcd*S23+2.0*Lad*Lcd*S13;

  //rescale for units of Lij^2 (fm^2 to cm^2)
  Intg *= 1.0e-26;

  return Intg;


/*  Attempts at a new implementation --- supressed for now
#if 1  // Sasview defaults
  if (icase <= 1) {
    N[0]=N[1]=1000.0;
    Phi[0]=Phi[1]=0.0000001;
    Kab=Kac=Kad=Kbc=Kbd=-0.0004;
    L[0]=L[1]=1.0e-12;
    v[0]=v[1]=100.0;
    b[0]=b[1]=5.0;
  } else if (icase <= 4) {
    Phi[0]=0.0000001;
    Kab=Kac=Kad=-0.0004;
    L[0]=1.0e-12;
    v[0]=100.0;
    b[0]=5.0;
  }
#else
  if (icase <= 1) {
    N[0]=N[1]=0.0;
    Phi[0]=Phi[1]=0.0;
    Kab=Kac=Kad=Kbc=Kbd=0.0;
    L[0]=L[1]=L[3];
    v[0]=v[1]=v[3];
    b[0]=b[1]=0.0;
  } else if (icase <= 4) {
    N[0] = 0.0;
    Phi[0]=0.0;
    Kab=Kac=Kad=0.0;
    L[0]=L[3];
    v[0]=v[3];
    b[0]=0.0;
  }
#endif

  const double Xa = q*q*b[0]*b[0]*N[0]/6.0;
  const double Xb = q*q*b[1]*b[1]*N[1]/6.0;
  const double Xc = q*q*b[2]*b[2]*N[2]/6.0;
  const double Xd = q*q*b[3]*b[3]*N[3]/6.0;

  // limit as Xa goes to 0 is 1
  const double Pa = Xa==0 ? 1.0 : -expm1(-Xa)/Xa;
  const double Pb = Xb==0 ? 1.0 : -expm1(-Xb)/Xb;
  const double Pc = Xc==0 ? 1.0 : -expm1(-Xc)/Xc;
  const double Pd = Xd==0 ? 1.0 : -expm1(-Xd)/Xd;

  // limit as Xa goes to 0 is 1
  const double Paa = Xa==0 ? 1.0 : 2.0*(1.0-Pa)/Xa;
  const double Pbb = Xb==0 ? 1.0 : 2.0*(1.0-Pb)/Xb;
  const double Pcc = Xc==0 ? 1.0 : 2.0*(1.0-Pc)/Xc;
  const double Pdd = Xd==0 ? 1.0 : 2.0*(1.0-Pd)/Xd;


  // Note: S0ij only defined for copolymers; otherwise set to zero
  // 0: C/D     binary mixture
  // 1: C-D     diblock copolymer
  // 2: B/C/D   ternery mixture
  // 3: B/C-D   binary mixture,1 homopolymer, 1 diblock copolymer
  // 4: B-C-D   triblock copolymer
  // 5: A/B/C/D quaternary mixture
  // 6: A/B/C-D ternery mixture, 2 homopolymer, 1 diblock copolymer
  // 7: A/B-C-D binary mixture, 1 homopolymer, 1 triblock copolymer
  // 8: A-B/C-D binary mixture, 2 diblock copolymer
  // 9: A-B-C-D tetra-block copolymer
#if 0
  const double S0aa = icase<5
                      ? 1.0 : N[0]*Phi[0]*v[0]*Paa;
  const double S0bb = icase<2
                      ? 1.0 : N[1]*Phi[1]*v[1]*Pbb;
  const double S0cc = N[2]*Phi[2]*v[2]*Pcc;
  const double S0dd = N[3]*Phi[3]*v[3]*Pdd;
  const double S0ab = icase<8
                      ? 0.0 : sqrt(N[0]*v[0]*Phi[0]*N[1]*v[1]*Phi[1])*Pa*Pb;
  const double S0ac = icase<9
                      ? 0.0 : sqrt(N[0]*v[0]*Phi[0]*N[2]*v[2]*Phi[2])*Pa*Pc*exp(-Xb);
  const double S0ad = icase<9
                      ? 0.0 : sqrt(N[0]*v[0]*Phi[0]*N[3]*v[3]*Phi[3])*Pa*Pd*exp(-Xb-Xc);
  const double S0bc = (icase!=4 && icase!=7 && icase!= 9)
                      ? 0.0 : sqrt(N[1]*v[1]*Phi[1]*N[2]*v[2]*Phi[2])*Pb*Pc;
  const double S0bd = (icase!=4 && icase!=7 && icase!= 9)
                      ? 0.0 : sqrt(N[1]*v[1]*Phi[1]*N[3]*v[3]*Phi[3])*Pb*Pd*exp(-Xc);
  const double S0cd = (icase==0 || icase==2 || icase==5)
                      ? 0.0 : sqrt(N[2]*v[2]*Phi[2]*N[3]*v[3]*Phi[3])*Pc*Pd;
#else  // sasview equivalent
//printf("Xc=%g, S0cc=%g*%g*%g*%g\n",Xc,N[2],Phi[2],v[2],Pcc);
  double S0aa = N[0]*Phi[0]*v[0]*Paa;
  double S0bb = N[1]*Phi[1]*v[1]*Pbb;
  double S0cc = N[2]*Phi[2]*v[2]*Pcc;
  double S0dd = N[3]*Phi[3]*v[3]*Pdd;
  double S0ab = sqrt(N[0]*v[0]*Phi[0]*N[1]*v[1]*Phi[1])*Pa*Pb;
  double S0ac = sqrt(N[0]*v[0]*Phi[0]*N[2]*v[2]*Phi[2])*Pa*Pc*exp(-Xb);
  double S0ad = sqrt(N[0]*v[0]*Phi[0]*N[3]*v[3]*Phi[3])*Pa*Pd*exp(-Xb-Xc);
  double S0bc = sqrt(N[1]*v[1]*Phi[1]*N[2]*v[2]*Phi[2])*Pb*Pc;
  double S0bd = sqrt(N[1]*v[1]*Phi[1]*N[3]*v[3]*Phi[3])*Pb*Pd*exp(-Xc);
  double S0cd = sqrt(N[2]*v[2]*Phi[2]*N[3]*v[3]*Phi[3])*Pc*Pd;
switch(icase){
  case 0:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    S0bb=0.000005;
    S0bc=0.000006;
    S0bd=0.000007;
    S0cd=0.000008;
    break;
  case 1:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    S0bb=0.000005;
    S0bc=0.000006;
    S0bd=0.000007;
    break;
  case 2:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    S0bc=0.000005;
    S0bd=0.000006;
    S0cd=0.000007;
    break;
  case 3:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    S0bc=0.000005;
    S0bd=0.000006;
    break;
  case 4:
    S0aa=0.000001;
    S0ab=0.000002;
    S0ac=0.000003;
    S0ad=0.000004;
    break;
  case 5:
    S0ab=0.000001;
    S0ac=0.000002;
    S0ad=0.000003;
    S0bc=0.000004;
    S0bd=0.000005;
    S0cd=0.000006;
    break;
  case 6:
    S0ab=0.000001;
    S0ac=0.000002;
    S0ad=0.000003;
    S0bc=0.000004;
    S0bd=0.000005;
    break;
  case 7:
    S0ab=0.000001;
    S0ac=0.000002;
    S0ad=0.000003;
    break;
  case 8:
    S0ac=0.000001;
    S0ad=0.000002;
    S0bc=0.000003;
    S0bd=0.000004;
    break;
  default : //case 9:
    break;
  }
#endif

  // eq 12a: \kappa_{ij}^F = \chi_{ij}^F - \chi_{i0}^F - \chi_{j0}^F
  const double Kaa = 0.0;
  const double Kbb = 0.0;
  const double Kcc = 0.0;
  //const double Kdd = 0.0;
  const double Zaa = Kaa - Kad - Kad;
  const double Zab = Kab - Kad - Kbd;
  const double Zac = Kac - Kad - Kcd;
  const double Zbb = Kbb - Kbd - Kbd;
  const double Zbc = Kbc - Kbd - Kcd;
  const double Zcc = Kcc - Kcd - Kcd;
//printf("Za: %10.5g %10.5g %10.5g\n", Zaa, Zab, Zac);
//printf("Zb: %10.5g %10.5g %10.5g\n", Zab, Zbb, Zbc);
//printf("Zc: %10.5g %10.5g %10.5g\n", Zac, Zbc, Zcc);

  // T = inv(S0)
  const double DenT = (- S0ac*S0bb*S0ac + S0ab*S0bc*S0ac + S0ac*S0ab*S0bc
                       - S0aa*S0bc*S0bc - S0ab*S0ab*S0cc + S0aa*S0bb*S0cc);
  const double T11 = (-S0bc*S0bc + S0bb*S0cc)/DenT;
  const double T12 = ( S0ac*S0bc - S0ab*S0cc)/DenT;
  const double T13 = (-S0ac*S0bb + S0ab*S0bc)/DenT;
  const double T22 = (-S0ac*S0ac + S0aa*S0cc)/DenT;
  const double T23 = ( S0ac*S0ab - S0aa*S0bc)/DenT;
  const double T33 = (-S0ab*S0ab + S0aa*S0bb)/DenT;

//printf("T1: %10.5g %10.5g %10.5g\n", T11, T12, T13);
//printf("T2: %10.5g %10.5g %10.5g\n", T12, T22, T23);
//printf("T3: %10.5g %10.5g %10.5g\n", T13, T23, T33);

  // eq 18e: m = 1/(S0_{dd} - s0^T inv(S0) s0)
  const double ZZ = S0ad*(T11*S0ad + T12*S0bd + T13*S0cd)
                  + S0bd*(T12*S0ad + T22*S0bd + T23*S0cd)
                  + S0cd*(T13*S0ad + T23*S0bd + T33*S0cd);

  const double m=1.0/(S0dd-ZZ);

  // eq 18d: Y = inv(S0)s0 + e
  const double Y1 = T11*S0ad + T12*S0bd + T13*S0cd + 1.0;
  const double Y2 = T12*S0ad + T22*S0bd + T23*S0cd + 1.0;
  const double Y3 = T13*S0ad + T23*S0bd + T33*S0cd + 1.0;

  // N = mYY^T + \kappa^F
  const double N11 = m*Y1*Y1 + Zaa;
  const double N12 = m*Y1*Y2 + Zab;
  const double N13 = m*Y1*Y3 + Zac;
  const double N22 = m*Y2*Y2 + Zbb;
  const double N23 = m*Y2*Y3 + Zbc;
  const double N33 = m*Y3*Y3 + Zcc;

//printf("N1: %10.5g %10.5g %10.5g\n", N11, N12, N13);
//printf("N2: %10.5g %10.5g %10.5g\n", N12, N22, N23);
//printf("N3: %10.5g %10.5g %10.5g\n", N13, N23, N33);
//printf("S0a: %10.5g %10.5g %10.5g\n", S0aa, S0ab, S0ac);
//printf("S0b: %10.5g %10.5g %10.5g\n", S0ab, S0bb, S0bc);
//printf("S0c: %10.5g %10.5g %10.5g\n", S0ac, S0bc, S0cc);

  // M = I + S0 N
  const double Maa = N11*S0aa + N12*S0ab + N13*S0ac + 1.0;
  const double Mab = N11*S0ab + N12*S0bb + N13*S0bc;
  const double Mac = N11*S0ac + N12*S0bc + N13*S0cc;
  const double Mba = N12*S0aa + N22*S0ab + N23*S0ac;
  const double Mbb = N12*S0ab + N22*S0bb + N23*S0bc + 1.0;
  const double Mbc = N12*S0ac + N22*S0bc + N23*S0cc;
  const double Mca = N13*S0aa + N23*S0ab + N33*S0ac;
  const double Mcb = N13*S0ab + N23*S0bb + N33*S0bc;
  const double Mcc = N13*S0ac + N23*S0bc + N33*S0cc + 1.0;
//printf("M1: %10.5g %10.5g %10.5g\n", Maa, Mab, Mac);
//printf("M2: %10.5g %10.5g %10.5g\n", Mba, Mbb, Mbc);
//printf("M3: %10.5g %10.5g %10.5g\n", Mca, Mcb, Mcc);

  // Q = inv(M) = inv(I + S0 N)
  const double DenQ = (+ Maa*Mbb*Mcc - Maa*Mbc*Mcb - Mab*Mba*Mcc
                       + Mab*Mbc*Mca + Mac*Mba*Mcb - Mac*Mbb*Mca);

  const double Q11 = ( Mbb*Mcc - Mbc*Mcb)/DenQ;
  const double Q12 = (-Mab*Mcc + Mac*Mcb)/DenQ;
  const double Q13 = ( Mab*Mbc - Mac*Mbb)/DenQ;
  //const double Q21 = (-Mba*Mcc + Mbc*Mca)/DenQ;
  const double Q22 = ( Maa*Mcc - Mac*Mca)/DenQ;
  const double Q23 = (-Maa*Mbc + Mac*Mba)/DenQ;
  //const double Q31 = ( Mba*Mcb - Mbb*Mca)/DenQ;
  //const double Q32 = (-Maa*Mcb + Mab*Mca)/DenQ;
  const double Q33 = ( Maa*Mbb - Mab*Mba)/DenQ;

//printf("Q1: %10.5g %10.5g %10.5g\n", Q11, Q12, Q13);
//printf("Q2: %10.5g %10.5g %10.5g\n", Q21, Q22, Q23);
//printf("Q3: %10.5g %10.5g %10.5g\n", Q31, Q32, Q33);
  // eq 18c: inv(S) = inv(S0) + mYY^T + \kappa^F
  // eq A1 in the appendix
  // To solve for S, use:
  //      S = inv(inv(S^0) + N) inv(S^0) S^0
  //        = inv(S^0 inv(S^0) + N) S^0
  //        = inv(I + S^0 N) S^0
  //        = Q S^0
  const double S11 = Q11*S0aa + Q12*S0ab + Q13*S0ac;
  const double S12 = Q12*S0aa + Q22*S0ab + Q23*S0ac;
  const double S13 = Q13*S0aa + Q23*S0ab + Q33*S0ac;
  const double S22 = Q12*S0ab + Q22*S0bb + Q23*S0bc;
  const double S23 = Q13*S0ab + Q23*S0bb + Q33*S0bc;
  const double S33 = Q13*S0ac + Q23*S0bc + Q33*S0cc;
  // If the full S is needed...it isn't since Ldd = (rho_d - rho_d) = 0 below
  //const double S14=-S11-S12-S13;
  //const double S24=-S12-S22-S23;
  //const double S34=-S13-S23-S33;
  //const double S44=S11+S22+S33 + 2.0*(S12+S13+S23);

  // eq 12 of Akcasu, 1990: I(q) = L^T S L
  // Note: eliminate cases without A and B polymers by setting Lij to 0
  // Note: 1e-13 to convert from fm to cm for scattering length
  const double sqrt_Nav=sqrt(6.022045e+23) * 1.0e-13;
  const double Lad = icase<5 ? 0.0 : (L[0]/v[0] - L[3]/v[3])*sqrt_Nav;
  const double Lbd = icase<2 ? 0.0 : (L[1]/v[1] - L[3]/v[3])*sqrt_Nav;
  const double Lcd = (L[2]/v[2] - L[3]/v[3])*sqrt_Nav;

  const double result=Lad*Lad*S11 + Lbd*Lbd*S22 + Lcd*Lcd*S33
                    + 2.0*(Lad*Lbd*S12 + Lbd*Lcd*S23 + Lad*Lcd*S13);

  return result;
*/
}
