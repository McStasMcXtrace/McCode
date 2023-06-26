double Iq(double q, double radius_effective, double volfraction, double perturb, double stickiness)
{
    double onemineps,eta;
    double sig,aa,etam1,etam1sq,qa,qb,qc,radic;
    double lam,lam2,test,mu,alpha,beta;
    double kk,k2,k3,ds,dc,aq1,aq2,aq3,aq,bq1,bq2,bq3,bq,sq;

    onemineps = 1.0-perturb;
    eta = volfraction/onemineps/onemineps/onemineps;

    sig = 2.0 * radius_effective;
    aa = sig/onemineps;
    etam1 = 1.0 - eta;
    etam1sq=etam1*etam1;
    //C
    //C  SOLVE QUADRATIC FOR LAMBDA
    //C
    qa = eta/6.0;
    qb = stickiness + eta/etam1;
    qc = (1.0 + eta/2.0)/etam1sq;
    radic = qb*qb - 2.0*qa*qc;
    if(radic<0) {
        //if(x>0.01 && x<0.015)
        //    Print "Lambda unphysical - both roots imaginary"
        //endif
        return(-1.0);
    }
    //C   KEEP THE SMALLER ROOT, THE LARGER ONE IS UNPHYSICAL
    radic = sqrt(radic);
    lam = (qb-radic)/qa;
    lam2 = (qb+radic)/qa;
    if(lam2<lam) {
        lam = lam2;
    }
    test = 1.0 + 2.0*eta;
    mu = lam*eta*etam1;
    if(mu>test) {
        //if(x>0.01 && x<0.015)
        // Print "Lambda unphysical mu>test"
        //endif
        return(-1.0);
    }
    alpha = (1.0 + 2.0*eta - mu)/etam1sq;
    beta = (mu - 3.0*eta)/(2.0*etam1sq);
    //C
    //C   CALCULATE THE STRUCTURE FACTOR
    //C
    kk = q*aa;
    k2 = kk*kk;
    k3 = kk*k2;
    SINCOS(kk,ds,dc);
    //ds = sin(kk);
    //dc = cos(kk);
    aq1 = ((ds - kk*dc)*alpha)/k3;
    aq2 = (beta*(1.0-dc))/k2;
    aq3 = (lam*ds)/(12.0*kk);
    aq = 1.0 + 12.0*eta*(aq1+aq2-aq3);
    //
    bq1 = alpha*(0.5/kk - ds/k2 + (1.0 - dc)/k3);
    bq2 = beta*(1.0/kk - ds/k2);
    bq3 = (lam/12.0)*((1.0 - dc)/kk);
    bq = 12.0*eta*(bq1+bq2-bq3);
    //
    sq = 1.0/(aq*aq +bq*bq);

    return(sq);
}