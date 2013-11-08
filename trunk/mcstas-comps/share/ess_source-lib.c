/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2013, All rights reserved
*         DTU Physics, Lyngby, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/ess_source-lib.c
*
* %Identification
* Written by: PW
* Date: Nov 7, 2013
* Origin: DTU Physics
* Release: McStas 2.1
* Version: 0.1
*
* This file is to be imported by the ESS_moderator_long component
* It defines a set of brilliance definitions (used via function pointer) for
* easier use of the component.
*
* Usage: within SHARE
* %include "ess_source-lib"
*
*******************************************************************************/

#ifndef ESS_SOURCE_LIB_H
#error McStas : please import this library with %include "ess_source-lib"
#endif

/* Base maths functions used below */
double Mezei_M(double l, double temp)
    {
      double a=949.0/temp;
      return 2*a*a*exp(-a/(l*l))/(l*l*l*l*l);
    }

/* This one is a bit strange - never used in the actual comp it seems? - Should rewrite.... */
double Mezei_F(double t, double tau, int n)
    {
      return (exp(-t/tau)-exp(-n*t/tau))*n/(n-1)/tau;
    }


/* This is the cold Mezei moderator from 2001 */
double ESS_Mezei_cold(double *t, double *p, double lambda, double tfocus_width, double tfocus_time, double dt, ess_moderator_struct extras) 
{
  // Spectrum related constants - ESS 2001 Cold moderator
  double T=50, tau=287e-6, tau1=0, tau2=20e-6, chi2=0.9, I0=8.21e11, I2=3.29e11, branch1=1, branch2=0.5, n2=5, n=20;
  
  // Branching
  double branch_tail=tau/ESS_SOURCE_DURATION;
  
  // Other variables
  double tail_flag, tau_l;
  
  // Taken directly from the ESS_moderator.comp:
  tail_flag = (rand01()<branch_tail);   /* Choose tail/bulk */
  if (tail_flag)
    {
      if (rand01() < branch2)
	{
	  if (tau1>0)
	    if (rand01() < branch1)     /* Quick and dirty non-general solution */
	      {  /* FIRST CASE a */
		tau_l = tau;
		*p = 1/(branch1*branch2*branch_tail); /* Correct for switching prob. */
	      }
	    else
	      {  /* FIRST CASE b */
		tau_l = tau1;
		*p = 1/((1-branch1)*branch2*branch_tail); /* Correct for switching prob. */
	      }
	  else
	    {
	      tau_l = tau;
	      *p = 1/(branch2*branch_tail); /* Correct for switching prob. */
	    }
	  *t = -tau_l*log(1e-12+rand01());       /* Sample from long-time tail a */
	  /* Correct for true pulse shape */
	  //	  p *= w_focus;                         /* Correct for target focusing */
	  *p *= tau_l/ESS_SOURCE_DURATION;                         /* Correct for tail part */
	  //p *= I0*w_mult*w_geom*Mezei_M(lambda,T);           /* Calculate true intensity */
	  *p *= I0*Mezei_M(lambda,T);
	}
      else
	{
	  /* SECOND CASE */
	  tau_l = tau2*lambda;
	  *t = -tau_l*log(1e-12+rand01());       /* Sample from long-time tail */
	  *p = n2/(n2-1)*((1-exp(-ESS_SOURCE_DURATION/tau_l))-(1-exp(-n2*ESS_SOURCE_DURATION/tau_l))*exp(-(n2-1)*(*t)/tau_l)/n);
	  /* Correct for true pulse shape */
	  *p /= (1-branch2)*branch_tail;          /* Correct for switching prob. */
	  *p *= tau_l/ESS_SOURCE_DURATION;                         /* Correct for tail part */
	  // p *= w_focus;                         /* Correct for target focusing */
	  //p *= I2*w_mult*w_geom/(1+exp(chi2*lambda-2.2))/lambda;                                         /* Calculate true intensity */
	  *p *= I2/(1+exp(chi2*lambda-2.2))/lambda;                                         /* Calculate true intensity */ 
	}
      *t += ESS_SOURCE_DURATION;                                 /* Add pulse length */
    }
  else /* Tail-flag */
    {
      if (tfocus_width>0) {
	*t = tfocus_time-dt;                    /* Set time to hit time window center */
	*t += randpm1()*tfocus_width/2.0;       /* Add random time within window width */
      } else {
	*t = ESS_SOURCE_DURATION*rand01();                        /* Sample from bulk pulse */
      }
      // FLAG to KILL these on return!

      /* if (t<0) ABSORB;                       /\* Kill neutron if outside pulse duration *\/ */
      /* if (t>ESS_SOURCE_DURATION) ABSORB; */
      if (rand01() < branch2)
	{
	  if (rand01() < branch1)     /* Quick and dirty non-general solution */
	    {  /* FIRST CASE a */
	      tau_l = tau;
	      *p = 1/(branch1*branch2*(1-branch_tail)); /* Correct for switching prob. */
	    }
	  else
	    {  /* FIRST CASE b */
	      tau_l = tau1;
	      *p = 1/((1-branch1)*branch2*(1-branch_tail)); /* Correct for switching prob. */
	    }
	  *p *= 1-n/(n-1)*(exp(-*t/tau_l)-exp(-n*(*t)/tau_l)/n); /* Correct for true pulse shape */
	  //	  p *= w_focus;                         /* Correct for target focusing */
	  if (tfocus_width>0) {
	    *p *= tfocus_width/ESS_SOURCE_DURATION;    	  	  /* Correct for time focusing */
	  }
	  //p *= I0*w_mult*w_geom*M(lambda,T);       /* Calculate true intensity */
	  *p *= I0*Mezei_M(lambda,T);       /* Calculate true intensity */
	}
      else
	{
	  /* SECOND CASE */
	  tau_l = tau2*lambda;
	  *p = 1-n2/(n2-1)*(exp(-*t/tau_l)-exp(-n2*(*t)/tau_l)/n2); /* Correct for true pulse shape */
	  *p /= (1-branch2)*(1-branch_tail);   /* Correct for switching prob. */
	  //p *= w_focus;                         /* Correct for target focusing */
	  if (tfocus_width) {
	    *p *= tfocus_width/ESS_SOURCE_DURATION;    		  /* Correct for time focusing */
	  }
	  //p *= I2*w_mult*w_geom/(1+exp(chi2*lambda-2.2))/lambda;    /* Calculate true intensity */
	  *p *= I2/(1+exp(chi2*lambda-2.2))/lambda;    /* Calculate true intensity */
	}
    }
  
} /* end of ESS_Mezei_cold */


/* This is the thermal Mezei moderator from 2001 */
double ESS_Mezei_thermal(double *t, double *p, double lambda, double tfocus_width, double tfocus_time, double dt, ess_moderator_struct extras)
{
  // Spectrum related constants - ESS 2001 Thermal moderator       
  double T=325, tau=80e-6, tau1=400e-6, tau2=12e-6, chi2=2.5, I0=13.5e11, I2=27.6e10, branch1=0.5, branch2=0.5, n2=5, n=20;

  // Branching
  double branch_tail=tau/ESS_SOURCE_DURATION;
  
  // Other variables
  double tail_flag, tau_l;
  
  // Taken directly from the ESS_moderator.comp:
  tail_flag = (rand01()<branch_tail);   /* Choose tail/bulk */
  if (tail_flag)
    {
      if (rand01() < branch2)
	{
	  if (tau1>0)
	    if (rand01() < branch1)     /* Quick and dirty non-general solution */
	      {  /* FIRST CASE a */
		tau_l = tau;
		*p = 1/(branch1*branch2*branch_tail); /* Correct for switching prob. */
	      }
	    else
	      {  /* FIRST CASE b */
		tau_l = tau1;
		*p = 1/((1-branch1)*branch2*branch_tail); /* Correct for switching prob. */
	      }
	  else
	    {
	      tau_l = tau;
	      *p = 1/(branch2*branch_tail); /* Correct for switching prob. */
	    }
	  *t = -tau_l*log(1e-12+rand01());       /* Sample from long-time tail a */
	  /* Correct for true pulse shape */
	  //	  p *= w_focus;                         /* Correct for target focusing */
	  *p *= tau_l/ESS_SOURCE_DURATION;                         /* Correct for tail part */
	  //p *= I0*w_mult*w_geom*Mezei_M(lambda,T);           /* Calculate true intensity */
	  *p *= I0*Mezei_M(lambda,T);
	}
      else
	{
	  /* SECOND CASE */
	  tau_l = tau2*lambda;
	  *t = -tau_l*log(1e-12+rand01());       /* Sample from long-time tail */
	  *p = n2/(n2-1)*((1-exp(-ESS_SOURCE_DURATION/tau_l))-(1-exp(-n2*ESS_SOURCE_DURATION/tau_l))*exp(-(n2-1)*(*t)/tau_l)/n);
	  /* Correct for true pulse shape */
	  *p /= (1-branch2)*branch_tail;          /* Correct for switching prob. */
	  *p *= tau_l/ESS_SOURCE_DURATION;                         /* Correct for tail part */
	  // p *= w_focus;                         /* Correct for target focusing */
	  //p *= I2*w_mult*w_geom/(1+exp(chi2*lambda-2.2))/lambda;                                         /* Calculate true intensity */
	  *p *= I2/(1+exp(chi2*lambda-2.2))/lambda;                                         /* Calculate true intensity */ 
	}
      *t += ESS_SOURCE_DURATION;                                 /* Add pulse length */
    }
  else /* Tail-flag */
    {
      if (tfocus_width>0) {
	*t = tfocus_time-dt;                    /* Set time to hit time window center */
	*t += randpm1()*tfocus_width/2.0;       /* Add random time within window width */
      } else {
	*t = ESS_SOURCE_DURATION*rand01();                        /* Sample from bulk pulse */
      }
      // FLAG to KILL these on return!

      /* if (t<0) ABSORB;                       /\* Kill neutron if outside pulse duration *\/ */
      /* if (t>ESS_SOURCE_DURATION) ABSORB; */
      if (rand01() < branch2)
	{
	  if (rand01() < branch1)     /* Quick and dirty non-general solution */
	    {  /* FIRST CASE a */
	      tau_l = tau;
	      *p = 1/(branch1*branch2*(1-branch_tail)); /* Correct for switching prob. */
	    }
	  else
	    {  /* FIRST CASE b */
	      tau_l = tau1;
	      *p = 1/((1-branch1)*branch2*(1-branch_tail)); /* Correct for switching prob. */
	    }
	  *p *= 1-n/(n-1)*(exp(-*t/tau_l)-exp(-n*(*t)/tau_l)/n); /* Correct for true pulse shape */
	  //	  p *= w_focus;                         /* Correct for target focusing */
	  if (tfocus_width>0) {
	    *p *= tfocus_width/ESS_SOURCE_DURATION;    	  	  /* Correct for time focusing */
	  }
	  //p *= I0*w_mult*w_geom*M(lambda,T);       /* Calculate true intensity */
	  *p *= I0*Mezei_M(lambda,T);       /* Calculate true intensity */
	}
      else
	{
	  /* SECOND CASE */
	  tau_l = tau2*lambda;
	  *p = 1-n2/(n2-1)*(exp(-*t/tau_l)-exp(-n2*(*t)/tau_l)/n2); /* Correct for true pulse shape */
	  *p /= (1-branch2)*(1-branch_tail);   /* Correct for switching prob. */
	  //p *= w_focus;                         /* Correct for target focusing */
	  if (tfocus_width) {
	    *p *= tfocus_width/ESS_SOURCE_DURATION;    		  /* Correct for time focusing */
	  }
	  //p *= I2*w_mult*w_geom/(1+exp(chi2*lambda-2.2))/lambda;    /* Calculate true intensity */
	  *p *= I2/(1+exp(chi2*lambda-2.2))/lambda;    /* Calculate true intensity */
	}
    }

} /* end of ESS_Mezei_thermal */


/* This is the Mezei moderator with a correction term from Klaus Lieutenant */
double ESS_2012_Lieutenant_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras)
{
  ESS_Mezei_cold(t, p,  lambda,  tfocus_w,  tfocus_t, tfocus_dt, extras);
  
  double cor;
  
  /* Correction factors to converts 'predicted' spectrum from cold moderator to the one observed in MCNPX */
  if (lambda<=2.5) cor=log(1.402+0.898*lambda)*(2.0776-4.1093*lambda+4.8836*pow(lambda,2)-2.4715*pow(lambda,3)+0.4521*pow(lambda,4));
  else if (lambda <= 3.5) cor = log(1.402 + 0.898*lambda)*(4.3369 - 1.8367*lambda + 0.2524*pow(lambda,2) );
  else if (lambda  > 3.5) cor = log(1.402 + 0.898*lambda);
  
  *p *=cor;

} /* end of ESS_2012_Lieutenant_cold */


/* This is the cold moderator with 2013 updates, fits from Troels Schoenfeldt */
/* Parametrization including moderator height for the "pancake" moderator */
double ESS_2013_Schoenfeldt_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras)
{
} /* end of ESS_2013_Schoenfeldt_cold */


/* This is the thermal moderator with 2013 updates, fits from Troels Schoenfeldt */
double ESS_2013_Schoenfeldt_thermal(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras)
{
} /* end of ESS_2013_Schoenfeldt_thermal */

/* Display of geometry - flat and TDR-like */
void ESS_mcdisplay_flat(double geometry)
{
}/* end of ESS_mcdisplay_flat */

void ESS_mcdisplay_TDRlike(double geometry)
{
}/* end of ESS_mcdisplay_TDRlike */


/* end of ess_source-lib.c */
