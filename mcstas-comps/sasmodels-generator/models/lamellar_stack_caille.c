/*	LamellarCaille kernel - allows for name changes of passed parameters ...

*/

static double
Iq(double qval,
   double del,
   double fp_Nlayers,
   double dd,
   double Cp,
   double sld,
   double solvent_sld)
{
  int Nlayers = (int)(fp_Nlayers+0.5);    //cast to an integer for the loop
  double contr;   //local variables of coefficient wave
  double inten,Pq,Sq,alpha,temp,t2;
  //double dQ, dQDefault, t1, t3;
  // from wikipedia 0.577215664901532860606512090082402431042159335
  const double Euler = 0.577215664901533;   // Euler's constant, increased sig figs for new models Feb 2015
  //dQDefault = 0.0;    //[=] 1/A, q-resolution, default value
  //dQ = dQDefault; // REMOVED UNUSED dQ calculations for new models Feb 2015

  contr = sld - solvent_sld;

  Pq = 2.0*contr*contr/qval/qval*(1.0-cos(qval*del));

  Sq = 0.0;
  for (int ii=1; ii < Nlayers; ii++) {
    temp = 0.0;
    alpha = Cp/4.0/M_PI/M_PI*(log(M_PI*ii) + Euler);
    //t1 = 2.0*dQ*dQ*dd*dd*alpha;
    t2 = 2.0*qval*qval*dd*dd*alpha;
    //t3 = dQ*dQ*dd*dd*ii*ii;

    temp = 1.0 - (double)ii / (double)Nlayers;
    //temp *= cos(dd*qval*ii/(1.0+t1));
    temp *= cos(dd*qval*ii);
    //temp *= exp(-1.0*(t2 + t3)/(2.0*(1.0+t1)) );
    temp *= exp(-t2/2.0);
    //temp /= sqrt(1.0+t1);

    Sq += temp;
  }

  Sq *= 2.0;
  Sq += 1.0;

  inten = 2.0*M_PI*Pq*Sq/(dd*qval*qval);

  inten *= 1.0e-04;   // 1/A to 1/cm

  return(inten);
}
