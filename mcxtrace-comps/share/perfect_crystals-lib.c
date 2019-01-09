#ifndef MXBRAGG_CRYSTALS_C
#define MXBRAGG_CRYSTALS_C

int MxBragg_DarwinReflectivityBC(double *Rsig, double *Rpi, double kh[3],
                         const double k0hat[3], const double nhat[3],
                         const double alpha[3],
                         double f00, double f0h, double fp, double fpp, double V, int h, int k, int l,
                         double debye_waller_B, double E,
                         int crystal_type, double fscaler, double fscalei){
    double lambda,theta, a, d;
    
    lambda = 2*PI/(E2K*E);  			/* wavelength in Å, E in keV, using built-in constants for safety    */
    a = cbrt(V); 				/* side length of unit cubic cell (Å)*/
    d = a/sqrt(h*h + k*k + l*l); 		/* d-spacing (Å)*/
    theta = asin(lambda/(2*d));  		/* kinematical bragg angle (rad) */
    double k0mag=2.0*M_PI/lambda; /* magnitude of k0 consistent with energy scale */
    double k0[3]={k0hat[0]*k0mag, k0hat[1]*k0mag, k0hat[2]*k0mag}; /* actual incoming k vector */
    double hscale=2.0*M_PI/d; /* minus sign to make it point into the crystal */
    double H[3]={alpha[0]*hscale, alpha[1]*hscale, alpha[2]*hscale};
    
    /* structure factor rules from:
     https://en.wikipedia.org/wiki/Structure_factor section on diamond cubic crystals
     */
    
    double complex fscaleh;
    double fscale0;
    
    switch(crystal_type) {
        case Bragg_crystal_explicit:
            /* use explicitly provided structure factor scale factor */
            break;
        case Bragg_crystal_diamond: /* diamond lattice rules */
            if (((h+k+l)%2) != 0){ 		/* (111) etc. odd sum eflection */
                fscaleh=4+4*I;
                fscale0=8;
            }
            else if (((h+k+l)%4)==0){ 		/* (400) etc. h+k+l=4n reflection */
                fscaleh=8;
                fscale0=8;
            } else {
                /* any other reflection is forbidden, will get a divide-by-zero somewhere, but user is
                 responsible for only using allowed reflections */
                fscaleh=0;
                fscale0=0;
            }
            break;
        case Bragg_crystal_fcc: /* fcc lattice rules */
        {
            int hpar=h%2, kpar=k%2, lpar=l%2;
            if ( hpar==kpar && kpar==lpar ) { /* all parities the same */
                fscaleh=4;
                fscale0=4;
            }
            else { 		/* mixed parity forbidden */
                fscaleh=0;
                fscale0=0;
            }
        }
            break;
        case Bragg_crystal_bcc: /* bcc lattice rules */
            if ( ((h+k+l)%2) == 0 ) { /* h+k+l even */
                fscaleh=2;
                fscale0=2;
            }
            else { 		/* otherwise forbidden */
                fscaleh=0;
                fscale0=0;
            }
            break;
        default:
            fscaleh=fscale0=0; /* fail later if unknown crystal type */
            break;
    }
    
    double complex F0=fscale0*((f00+fp)+fpp*I); // NEVER CHECKED for crystals other than diamond structure
    double complex Fh=cabs(fscaleh)*((f0h+fp)+fpp*I);
    
    double GAMMA=RE*lambda*lambda/(PI*V);
    double M=debye_waller_B*SQR(sin(theta)/lambda)*(2./3.); /* isotropic temperature factor */
    double complex chi0 = -F0*GAMMA;
    double complex chih = -Fh*GAMMA*exp(-M);
    
    /* now we have to solve for the free-space kh vector outside the crystal.
     The requirement is that kh = Kh + e * nhat, and is pure real and has the same magnitude as k0.
     Thus, kh = k0 + kq*nhat + H + e*nhat, |kh| = |k0|, and kq+e must be real to make kh real.
     if kq+e == q,
     q^2 + H^2 + 2 q (k0+H).nhat + 2 k0.H = 0
     Note that this is independent of the messy dispersion relationship, and the same for both polarizations.
     */
    double k0plusH[3];
    vplus(k0plusH, k0, H); // temporary k0+H as needed in second poly term above
    double bb=2*vdot(k0plusH, nhat);
    double cc = vdot(H,H) +2*vdot(k0,H);
    double root1, root2;
    qsolve(root1, root2, 1., bb, cc, sqrt); //root2 is the small root
    vplus(kh, k0plusH, root2*nhat);

    /* compute the external asymmetry b0 which is the ratio of the sizes of the footprint of the beam on the crystal */
    double b0 = vdot(k0, nhat)/vdot(kh, nhat); // since |k0|=|kh| from above, we don't have to worry about the normalization
    
    for (int i=0; i<2; i++) { // do reflectivity using shared geometry for both polarizations
        double C=(i==0)?fabs(cos(2*theta)) : 1; // polarization factor
        double complex xi0;
        double K0[3], Kh[3];
        double complex kqvals[4], xi0vals[4], xihvals[4];
        
        // warning: valid only for centrosymmetric crystals here where chih=chihbar
        int fail=MxBragg_DiffractionDispersion(kqvals, xi0vals, xihvals,
                   k0, nhat, H, chi0, chih*chih, C, 1); // get first (interesting) root only

        double complex kq=kqvals[0];
        xi0=xi0vals[0];
        vplus(K0, k0, creal(kq)*nhat);
        vplus(Kh, K0, H);
        
        if(fail) {
            *Rsig=0;
            *Rpi=0;
            return 1;
        }
        
        double complex eq24=2*xi0/(k0mag*C*chih); // B&C equation 24
        double eratio=cabs(eq24);
        
#ifdef MCDEBUG
        fprintf(stderr,
                "Bragg Geometry results: k0=(%.3f %.3f %.3f), kh=(%.3f %.3f %.3f) "
                "xi0 = (%.3e + %.3e i) "
                "b0=%.4f "
                "eq24 = (%.3e + %.3e i) "
                "R = %.3e "
                "\n",
                k0[0], k0[1], k0[2], kh[0], kh[1], kh[2],
                creal(xi0), cimag(xi0),
                b0,
                creal(eq24), cimag(eq24), eratio*eratio/fabs(b0));
#endif
        
        if(i==0)  *Rpi=eratio*eratio/fabs(b0); // the fabs(b0) is the footprint correction
        else      *Rsig=eratio*eratio/fabs(b0);
    }
    return 0;
}

void cross(double res[3], const double v1[3], const double v2[3], int unitize)
{   // use our own cross product which takes arrays directly to reduce pointer shuffling
    res[0]=v1[1]*v2[2]-v1[2]*v2[1];
    res[1]=v1[2]*v2[0]-v1[0]*v2[2];
    res[2]=v1[0]*v2[1]-v1[1]*v2[0];
    if(unitize) {
        double l=sqrt(res[0]*res[0]+res[1]*res[1]+res[2]*res[2]);
        res[0]/=l;
        res[1]/=l;
        res[2]/=l;
    }
}
    
int MxBragg_DiffractionDispersion(double complex kqvals[4], double complex xi0[4], double complex xih[4],
                          const double k0[3], const double nhat[3],
                          const double H[3],
                          double complex chi0, double complex chih_chihbar, double C, int nroots){
    /* compute the Batterman & Cole eq. 17 dispersion relation, and associated quantities.
       nroots sets how many roots to find.  They are sorted, with the first root being the attenuated incoming ray,
       then the  amplfied incoming ray, then the two 'big roots'.
       Normally only nroots=1 is interesting for Bragg diffraction.
     */

    double k0mag=sqrt(vdot(k0,k0));
    
    /* B&C equation 17 is ( |k0 + kq nhat|^2 - k0^2(1+chi0) ) ( |k0 + kq nhat + H|^2 - k0^2(1+chi0) ) = k^4 C^2 chih chihbar */
    double k0plusH[3];
    vplus(k0plusH, k0, H); // temporary k0+H as needed in second poly term above
    double complex poly1[3], poly2[3], poly[5], deriv[4];
    poly1[2]=1;
    poly1[1]=2*vdot(k0,nhat);
    poly1[0]=-k0mag*k0mag*chi0;
    poly2[2]=1.;
    poly2[1]=2*vdot(k0plusH,nhat);
    poly2[0]=-k0mag*k0mag*chi0+vdot(H,H)+2*vdot(k0,H);
    /* for multiplying these polys, just write it out explicitly */
    poly[0]=poly1[0]*poly2[0];
    poly[1]=poly1[0]*poly2[1]+poly1[1]*poly2[0];
    poly[2]=poly1[0]*poly2[2]+poly1[1]*poly2[1]+poly1[2]*poly2[0];
    poly[3]=poly1[1]*poly2[2]+poly1[2]*poly2[1];
    poly[4]=poly1[2]*poly2[2];
    double complex epsilon=(k0mag*k0mag*k0mag*k0mag*C*C)*chih_chihbar; // right-hand side of dispersion
    poly[0]-=epsilon;
    deriv[3]=4*poly[4];
    deriv[2]=3*poly[3];
    deriv[1]=2*poly[2];
    deriv[0]=1*poly[1];
    
    /* the dispersion polynomial is p1*p2-epsilon = 0; first, find big roots and factor out to leave only a quadratic with small (interesting) roots */
    /* big roots will be _close_ to big roots of p1 and p2, use these as guesses */
    double complex p1b, p1s, p2b, p2s;
    // p1b and p2b are always larger root in magnitude
    qsolve(p1b, p1s, poly1[2], poly1[1], poly1[0], csqrt);
    qsolve(p2b, p2s, poly2[2], poly2[1], poly2[0], csqrt);
    
    /* now, (x-p1b)(x-p2b)(x-p1s)(x-p2s)-epsilon is the full poly, but near the interesting region around p1s, p2s, flatten first two terms */
    /* so x0 = (p1s+p2s)*0.5, this is (x-p1s)(x-p2s)-epsilon/((x0-p1b)(x0-p2b)) = 0 to get approximate roots of full poly */
    double complex x0=(p1s+p2s)*0.5;
    double complex fp[3] = {p1s*p2s-epsilon/((x0-p1b)*(x0-p2b)), -(p1s+p2s), 1.0 };
    // factored poly roots, should be very close to exact roots
    // practical note:  looking at convergence, these roots are so close, one could probably drop the Newton's method refinement
    // completely.  However, since this code is intended to be pedantically correct, I will do the refinement.
    double complex initroots[4];
    qsolve(initroots[0], initroots[1], fp[2], fp[1], fp[0], csqrt); // solution to factored polynomial is starting point for newton's method
    
    int rootloops, fail=0;
    // make sure the first root has im(k).re(k) < 0 so beam is attenuated. Since re(k) is nearly k0 and im(k) is im(kq)*nhat,
    // we need k0.nhat * im(k) < 0
    double kdotnhat=vdot(k0, nhat);
    if(cimag(initroots[0])*kdotnhat > 0) {
        if(cimag(initroots[1])*kdotnhat < 0) {
            double complex swap;
            swap=initroots[0]; initroots[0]=initroots[1]; initroots[1]=swap;
        } else {
            // neither root has im(k) < 0, no physically possible solution.  Should never happen.
            fprintf(stderr, "PerfectCrystal: No attenuating solution for incoming vector found, r1=(%.3e + %.3e i) 	r2=(%.3e + %.3e i) \n",
                    creal(initroots[0]), cimag(initroots[0]), creal(initroots[1]), cimag(initroots[1]));
            exit(1);
        }
    }
    initroots[2]=p1b;
    initroots[3]=p2b; // the two big roots are last in the list
    
    for(rootloops=0; rootloops < nroots; rootloops++) {
        double complex dd, kq, pv, dv;
        kq=initroots[rootloops];
#if MCDEBUG
        fprintf(stderr,"Batterman Cole dispersion Newton Iterations, starting at root =(%.3e + %.3e i) \n",
                creal(kq), cimag(kq)
                );
#endif
        int stepcount=0;
        do {
            pv=(((poly[4]*kq+poly[3])*kq+poly[2])*kq+poly[1])*kq+poly[0]; // poly value
            dv=((deriv[3]*kq+deriv[2])*kq+deriv[1])*kq+deriv[0]; // derivative value
            dd=-pv/dv; // Newton's method step
            kq+=dd;
            stepcount++;
#ifdef MCDEBUG_EXTRA
            fprintf(stderr,"Batterman Cole dispersion Newton Iterations, step count=%d, pv=%.3e dv=%.3e "
                    "kq=(%.3e + %.3e I) shift=%.3e\n",
                    stepcount,
                    cabs(pv), cabs(dv), creal(kq), cimag(kq), cabs(dd) );
#endif
        } while ( ((cabs(dd) > 1e-15) && (stepcount < 20)) );
        kqvals[rootloops]=kq;
        
        fail= (stepcount == 20);
        if(fail) { // should never happen. always converges in about 3 steps!
            fprintf(stderr,"****Newton's method convergence failure in Bragg_Geometry, killing particle!\n");
            kq=initroots[rootloops]; // leave offset plausible, close to quadratic start
        }
        
        xi0[rootloops]=((poly1[2]*kq+poly1[1])*kq+poly1[0])/(2*k0mag);
        xih[rootloops]=((poly2[2]*kq+poly2[1])*kq+poly2[0])/(2*k0mag);
    }
    return fail;
}
    
void MxBragg_DarwinReflectivity(double *R, double *Thetah, double *Theta0, double *DeltaTheta0,
                             double f00, double f0h, double fp, double fpp, double V, double alpha, int h, int k, int l,
                             double debye_waller_B, double E, double Thetain, int pol,
                            int crystal_type, double fscaler, double fscalei
                             )
{

    double lambda,theta,theta0,DeltaThetas,a,d,b,C,W,kappa,g,L;
    double F0r,F0i,Fhr,Fhi,psi0r,psi0i,psihr,psihi;

    lambda = 2*PI/(E2K*E);  			/* wavelength in Å, E in keV, using built-in constants for safety    */
    a = cbrt(V); 				/* side length of unit cubic cell (Å)*/
    d = a/sqrt(h*h + k*k + l*l); 		/* d-spacing (Å)*/
    theta = asin(lambda/(2*d));  		/* kinematical bragg angle (rad) */
    b = sin(theta + alpha)/sin(theta - alpha);  /* asymmetry factor */

    *Theta0 = Thetain - alpha; 			/* (rad) angle between Bragg planes and incident ray */
    *Thetah = b*(*Theta0 - theta) + theta;   	/* (rad) Angle betweeb Bragg planes and reflected ray */
    /*check if Bragg angle is less than alpha. If so return 0 reflectivity*/
    if (theta<alpha) {
        *R=0;
        *DeltaTheta0 = -1; /*to mark it irrelevant*/
    }

    /* Define polarization factor: */
    switch(pol){
        case 0:
            C = (1 + fabs(cos(2*theta)))/2;         	/* unpolarized */
            break;
        case 1:
            C = fabs(cos(2*theta));  		/* polarization in the scattering plane */
            break;
        case 2:
            C = 1;                          	/* polarization perpendicular to the scattering plane*/
            break;
    }

    /* structure factor rules from:
https://en.wikipedia.org/wiki/Structure_factor section on diamond cubic crystals
     */

    switch(crystal_type) {
        case Bragg_crystal_explicit:
            /* use explicitly provided structure factor scale factor */
            break;
        case Bragg_crystal_diamond: /* diamond lattice rules */
            if (((h+k+l)%2) != 0){ 		/* (111) etc. odd sum eflection */
                fscaler=fscalei=4.0;
            }
            else if (((h+k+l)%4)==0){ 		/* (400) etc. h+k+l=4n reflection */
                fscaler=8; fscalei=0;
            } else {
                /* any other reflection is forbidden, will get a divide-by-zero somewhere, but user is
                   responsible for only using allowed reflections */
                fscaler=0; fscalei=0;
            }
            break;
        case Bragg_crystal_fcc: /* fcc lattice rules */
            {
                int hpar=h%2, kpar=k%2, lpar=l%2;
                if ( hpar==kpar && kpar==lpar ) { /* all parities the same */
                    fscaler=4.0; fscalei=0.0;
                }
                else { 		/* mixed parity forbidden */
                    fscaler=0; fscalei=0;
                }
            }
            break;
        case Bragg_crystal_bcc: /* bcc lattice rules */
            if ( ((h+k+l)%2) == 0 ) { /* h+k+l even */
                fscaler=2.0; fscalei=0.0;
            }
            else { 		/* otherwise forbidden */
                fscaler=0; fscalei=0;
            }
            break;
        default:
            fscaler=0; fscalei=0; /* fail later if unknown crystal type */
            break;
    }

    F0r=8*(f00+fp);
    F0i=8*fpp;
    double scalemag=sqrt(fscaler*fscaler+fscalei*fscalei);
    Fhr=scalemag*(f0h+fp);
    Fhi=scalemag*fpp;

    double main_scale=RE*lambda*lambda/(PI*V);
    double M=debye_waller_B*SQR(sin(Thetain)/lambda)*(2./3.); /* temperature factor */
    psi0r = F0r*main_scale;
    psi0i = F0i*main_scale;
    psihr = Fhr*main_scale*exp(-M); /* Eq 23*/
    psihi = Fhi*main_scale*exp(-M); /* only angle-dependent part gets scaled by temp factor */

    W = 0.5 * (sqrt(b) + 1/sqrt(b)) * psi0r/(C * psihr) +  sqrt(b)*sin(2*theta)*(theta - *Theta0)/(C * psihr); /* eq 28*/
    kappa = psihi/psihr;                                              	/* eq 22 */
    g = 0.5*(sqrt(b) + 1/sqrt(b))*psi0i/(C*psihr);               	/* eq 21 */
    L = (1/(1 + kappa*kappa))*( W*W + g*g + sqrt(SQR(W*W - g*g - 1 + kappa*kappa) + 4*SQR(g*W - kappa)));

    /* *R = L - sqrt(L*L - 1); */
    /* replace x-sqrt(x^2-1) with exactly equal 1/(x+sqrt(x^2-1)) to avoid roundoff when x is large ... MHM */
    *R = 1/(L + sqrt(L*L - 1));

    DeltaThetas = psi0r/sin(2*theta);               	/* eq 32 */
#ifdef MCDEBUG
    printf("E,lambda= %f , %f \n",E,lambda);
    printf("theta= %f \n",theta*180/PI);
    printf("Theta0= %f \n",*Theta0*180/PI);
    printf("theta = %g rad, alpha=%g rad.\n",theta,alpha);
    printf("b,sqrt(b)= %f %f\n",b,sqrt(b));
    printf("1/sqrt(b)= %f \n",1/sqrt(b));
    printf("Fhr, Fhi, F0r, F0i= %g %g %g %g\n",Fhr, Fhi, F0r, F0i);
    printf("psihr, psihi, psi0r, psi0i= %g %g %g %g\n",psihr, psihi, psi0r, psi0i);
    printf("sqrt(b)*sin(2*theta)= %g \n",sqrt(b)*sin(2*theta));
    printf("C, pis0r,C * psihr= %g %g %g\n",C, psi0r,C * psihr);
    printf("W= %f \n",W);
    printf("kappa= %f \n",kappa);
    printf("g= %f \n",g);
    printf("L= %f \n",L);
    printf("R= %f \n",*R);
    printf("DeltaThetas %f \n",3600*DeltaThetas*180/PI);
#endif
    *DeltaTheta0 = 0.5*(1 + 1/b)*DeltaThetas;                        	/* center of reflectivity curve is at theta + DeltaTheta0 eq 31 */
}


#endif /* MXBRAGG_CRYSTALS_BC_C SHARE section */
