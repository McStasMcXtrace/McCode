/********************************************************************************************************************************************************/
/*  VITESS module 'chopper_fermi'                                                            
/*                                                                                           
/* The free non-commercial use of these routines is granted providing due credit is given to 
/* the authors.                                                                              
/*                                                                                           
/* 1.00  Jul 2002  G. Zsigmond	initial version                                             
/* 1.01  Aug 2002  G. Zsigmond	forward all coordinates                                     
/* 1.02  Sep 2002  G. Zsigmond	included more channel windows                               
/* 1.03  Apr 2003  G. Zsigmond	sign correction                                             
/* 1.04  May 2003  G. Zsigmond	info changed                                                
/* 1.05  Jun 2003  G. Zsigmond	modulo function included for safety                         
/* 1.06  Jul 2003  G. Zsigmond	generalised for optional number of pulses; warnings included
/* 1.07  Oct 2003  G. Zsigmond	superfluous modulo function cancelled                       
/* 1.08  Nov 2003  G. Zsigmond	put 2 more windows representing channels, now 6 windows    
/*                               in the big IF loop ">=" changed to ">"                      
/* 1.09  Jan 2004  K. Lieutenant changes for 'instrument.dat'                                
/* 1.10  Jan 2004  G. Zsigmond   back to 4 windows representing channels
/* 1.11  Apr 2004  G. Zsigmond   negative time of flight defined
/* 1.12  Apr 2004  G. Zsigmond   negative time of flight - corrections, set zero time
/* 1.13  May 2004  G. Zsigmond   circular geom option and channel length included in curved fc
/* 1.14  JUL 2004  G. Zsigmond   small change in Init to adapt to new GUI
/* 1.15  OCT 2004  G. Zsigmond   changed to use both even or odd number of channels
/* 1.16  MAY 2005  G. Zsigmond   output changed to give trajectory coordinates at a plane crossing the center of the chopper (to be compatible with zero time option)
/*	                              zero time option fixed to get one peak 
/*                               shadowing cylinder opening activated 
/* 1.17  MAY 2005  G. Zsigmond  new option choice of 4, 6(better,slower) or 8(much better, very slow) gates, 4 gates option adjusted
/* 1.18  MAY 2005  K. Lieutenant changes to use this module in McStas as well               
/********************************************************************************************************************************************************/

#if defined VITESS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "general.h"
#include "init.h"
#include "intersection.h"
#include "softabort.h"

/* START HEADER STORY */

/* input parameters */
int        Ngates=4,    /* Number of gates forming the channel: 4 (default), 6 or 8 */
           zerotime=0;     /* option: set time (close to) zero                    -z */
long       Nchannels;      /* number of channels of the Fermi chopper             -l */
double     omega,          /* frequency of rotation                       [1/s]   -n */
           height,         /* height of the Fermi chopper                  [cm]   -a */
           width,          /* width of the Fermi chopper                   [cm]   -b */
           depth,          /* channel length of the Fermi chopper          [cm]   -c */
           optimal_wl,     /* wavelength of highest transmission          [Ang]   -L */
           diameter,       /* diameter of the shadowing cylinder           [cm]   -r */
           Phase,          /* dephasing angle at zero time                [deg]   -l */
           wallwidth;      /* thickness of walls separating the channels          -m */
char       sGeomFileName[512];/* name of output file for geometry information        -G */
VectorType pos_ch;         /* centre position of the Fermi chopper        [cm] X,Y,V */

/* other global parameters */
#include "chopper_fermi.h"

/* prototypes and small functions */
void		OutputTransformations(double *tof, double *wl, double *prob, VectorType Pos, VectorType Dir, VectorType SpinVector);
void		ReadParameterFile();
void		ChopperFermiInit(int argc, char *argv[]);
void		ChopperFermiCleanup();
double	asinplus(double val);
double	asinminus(double val);

double		asin2PI(double val)
{ double result;
  if (val>=0) result = (double)asin(val);
  else  result =  2*M_PI + (double)asin(val);
  return result;
}

/* FINISH HEADER STORY */


int main(int argc, char **argv)
{
  long	 i;                 /* index of trajectory      */

  /* Initialize the program according to the parameters given  */

  Init(argc, argv, VT_CHOP_FERMI);
  print_module_name("Fermi-Chopper 1.17m");
  ChopperFermiInit(argc, argv);


  /* Get the neutrons from the file */
  DECLARE_ABORT;

  while((ReadNeutrons())!= 0)
    {
      CHECK;	/* here is what happens to the neutron */

      for(i=0;i<NumNeutGot;i++)

	{
	  CHECK;
#endif

#if defined VITESS || defined MCSTAS_TRACE
	  TOF = InputNeutrons[i].Time;

	  WL = InputNeutrons[i].Wavelength;

	  CopyVector(InputNeutrons[i].Position, Pos); 

	  CopyVector(InputNeutrons[i].Vector, Dir); 

	  Dir[0]	= (double)sqrt(1 - sq(Dir[1]) - sq(Dir[2])); 


	  /* shift to center of Fermi-Chopper */
#ifdef VITESS
	  SubVector(Pos, pos_ch);
#endif

		/*trajectories which do not intersect the entrance and exit window */

	  {double pos[3], n[3];

		  n[1]=n[2]=0.; n[0]=1.;
		  if((PlaneLineIntersect(Pos, Dir, n, - diameter/2., pos))==1)
			{
				  if((pos[2]>=height/2.)||(pos[2]<= - height/2.)||(pos[1]>=diameter/2.)||(pos[1]<= - diameter/2.)) ABSORB;
			}
		  else ABSORB;

		  if((PlaneLineIntersect(Pos, Dir, n, diameter/2., pos))==1)
			{
				  if((pos[2]>=height/2.)||(pos[2]<= - height/2.)||(pos[1]>=diameter/2.)||(pos[1]<= - diameter/2.)) ABSORB;
			}
		  else ABSORB;
	

		/* translates neutron variables for X'= - diameter/2.  */

		{
			VectorType Path;

			TOF = TOF + (- diameter/2. - Pos[0]) / fabs(Dir[0]) / V_FROM_LAMBDA(WL); 
			
			if((TOF<0)&&(Nchannels==1)){fprintf(LogFilePtr,"\nERROR: Single-slit Fermi chopper needs positive flight time at the chopper position! \n"); exit(-1); }
				
			CopyVector(Dir, Path);

			MultiplyByScalar(Path, (- diameter/2. - Pos[0])/ Dir[0] );

			AddVector(Pos, Path);  
		}							/*	 Path = displacement vector */

	
	/* calculate time entering-edge and exiting-edge of 4 windows along the channels */

	  {	long j, k, m;
	  double sq_D_0_1, sq_term, omega_fact, dirpos, vz_pos, phase[10][2000];
 
	  sq_D_0_1 = sq(Dir[0]) + sq(Dir[1]);
	  dirpos = Dir[0]*Pos[1] - Dir[1]*Pos[0];
	  sq_term  = sq(dirpos) / sq_D_0_1;
	  omega_fact = omega / (V_FROM_LAMBDA(WL) * Dir[0]);
	  vz_pos = Pos[1] > 0.0 ? 1.0 : -1.0;

	  phase0 = fmod(Phase + omega*TOF, coef_pi*M_PI); 
	
	  for(k=0; k<Ngates; k++) 
	  {
		for(j=0; j < 2*Nchannels+2; j++) {

		  double x_ch_k_j, y_ch_k_j, sq_x_ch_k_j, Denom_k, Arg_k, arg_k, pha_k_j, y_ch_new_k_j;

		  x_ch_k_j    = x_ch[k][j];
		  y_ch_k_j    = y_ch[k][j];
		  sq_x_ch_k_j = sq(x_ch_k_j);
		  Denom_k     = sqrt( sq_D_0_1 * (sq_x_ch_k_j + sq(y_ch_k_j)) );

		  Arg_k = dirpos / Denom_k;

		  if (fabs(Arg_k) > 1.) {
			Arg_k = vz_pos;
			y_ch_new_k_j = Arg_k * sqrt(sq_term - sq_x_ch_k_j);
		  } else
			y_ch_new_k_j = y_ch_k_j; /* no intersection with trajectory */

		  Denom_k = sqrt( sq_D_0_1 * (sq_x_ch_k_j + sq(y_ch_new_k_j)) );

		  arg_k = (Dir[0]*y_ch_new_k_j - Dir[1]*x_ch_k_j) / Denom_k;

		  if (fabs(arg_k) > 1.) {
			phase[k][j] = 777;} 
			  
		  else {
			pha_k_j = asin(Arg_k) - asin(arg_k); 

			if(x_ch_k_j < 0.) pha_k_j = - pha_k_j; 
								
			phase[k][j] =  pha_k_j - omega_fact * (x_ch_k_j * cos(pha_k_j) - y_ch_new_k_j * sin(pha_k_j) - Pos[0]);
		  }
		}
	  }

	  if(Ngates==4){
		  m = -1; 

		  for(j=0;j<2*Nchannels+1; j++) 
		  {
					if((m == 1)&&(phase[0][j] < phase0 )&&(phase0 < phase[0][j+1])
							   &&(phase[1][j] < phase0 )&&(phase0 < phase[1][j+1])
							   &&(phase[2][j] > phase0 )&&(phase0 > phase[2][j+1])
							   &&(phase[3][j] > phase0 )&&(phase0 > phase[3][j+1]))
					{/*fprintf(LogFilePtr, "j  %d   phases %f   %f    %f\n", j, 57.296*phase[0][j], 57.296*phase[0][j+1], 57.296*phase0);*/ 
											 goto happyend;}

					m = m * (-1); 
		  }
		  
		  
		  /* also tries one turn earlier  */
		  
		  if((phase0 > 0)&&(omega > 0)) phase0 +=  - coef_pi*M_PI;
		  if((phase0 < 0)&&(omega < 0)) phase0 +=  coef_pi*M_PI;

		  m = -1; 

		  for(j=0;j<2*Nchannels+1; j++) 
		  {
					if((m == 1)&&(phase[0][j] < phase0 )&&(phase0 < phase[0][j+1])
							   &&(phase[1][j] < phase0 )&&(phase0 < phase[1][j+1])
							   &&(phase[2][j] > phase0 )&&(phase0 > phase[2][j+1])
							   &&(phase[3][j] > phase0 )&&(phase0 > phase[3][j+1]))
					{/*fprintf(LogFilePtr, "j  %d   phases %f   %f    %f\n", j, 57.296*phase[0][j], 57.296*phase[0][j+1], 57.296*phase0);*/ 
											 goto happyend;}

					m = m * (-1); 
		  }
	  }

	  if(Ngates==6){
		  m = -1; 

		  for(j=0;j<2*Nchannels+1; j++) 
		  {
					if((m == 1)&&(phase[0][j] < phase0 )&&(phase0 < phase[0][j+1])
							   &&(phase[1][j] < phase0 )&&(phase0 < phase[1][j+1])
							   &&(phase[2][j] < phase0 )&&(phase0 < phase[2][j+1])
							   &&(phase[3][j] > phase0 )&&(phase0 > phase[3][j+1])
							   &&(phase[4][j] > phase0 )&&(phase0 > phase[4][j+1])
							   &&(phase[5][j] > phase0 )&&(phase0 > phase[5][j+1]))
					{goto happyend;}
											 
					m = m * (-1); 
		  }
		  
		  /* also tries one turn earlier  */
		  
		  if((phase0 > 0)&&(omega > 0)) phase0 +=  - coef_pi*M_PI;
		  if((phase0 < 0)&&(omega < 0)) phase0 +=  coef_pi*M_PI;

		  m = -1; 

		  for(j=0;j<2*Nchannels+1; j++) 
		  {
					if((m == 1)&&(phase[0][j] < phase0 )&&(phase0 < phase[0][j+1])
							   &&(phase[1][j] < phase0 )&&(phase0 < phase[1][j+1])
							   &&(phase[2][j] < phase0 )&&(phase0 < phase[2][j+1])
							   &&(phase[3][j] > phase0 )&&(phase0 > phase[3][j+1])
							   &&(phase[4][j] > phase0 )&&(phase0 > phase[4][j+1])
							   &&(phase[5][j] > phase0 )&&(phase0 > phase[5][j+1]))
					{goto happyend;}
											 
					m = m * (-1); 
		  }
	  }

	  if(Ngates==8){
		  m = -1; 

		  for(j=0;j<2*Nchannels+1; j++) 
		  {
					if((m == 1)&&(phase[0][j] < phase0 )&&(phase0 < phase[0][j+1])
							   &&(phase[1][j] < phase0 )&&(phase0 < phase[1][j+1])
							   &&(phase[2][j] < phase0 )&&(phase0 < phase[2][j+1])
							   &&(phase[3][j] < phase0 )&&(phase0 < phase[3][j+1])
							   &&(phase[4][j] > phase0 )&&(phase0 > phase[4][j+1])
							   &&(phase[5][j] > phase0 )&&(phase0 > phase[5][j+1])
							   &&(phase[6][j] > phase0 )&&(phase0 > phase[6][j+1])
							   &&(phase[7][j] > phase0 )&&(phase0 > phase[7][j+1]))
					{goto happyend;}
											 
					m = m * (-1); 
		  }
		  
		  /* also tries one turn earlier  */
		  
		  if((phase0 > 0)&&(omega > 0)) phase0 +=  - coef_pi*M_PI;
		  if((phase0 < 0)&&(omega < 0)) phase0 +=  coef_pi*M_PI;

		  m = -1; 

		  for(j=0;j<2*Nchannels+1; j++) 
		  {
					if((m == 1)&&(phase[0][j] < phase0 )&&(phase0 < phase[0][j+1])
							   &&(phase[1][j] < phase0 )&&(phase0 < phase[1][j+1])
							   &&(phase[2][j] < phase0 )&&(phase0 < phase[2][j+1])
							   &&(phase[3][j] < phase0 )&&(phase0 < phase[3][j+1])
							   &&(phase[4][j] > phase0 )&&(phase0 > phase[4][j+1])
							   &&(phase[5][j] > phase0 )&&(phase0 > phase[5][j+1])
							   &&(phase[6][j] > phase0 )&&(phase0 > phase[6][j+1])
							   &&(phase[7][j] > phase0 )&&(phase0 > phase[7][j+1]))
					{goto happyend;}
											 
					m = m * (-1); 
		  }
	  }

					ABSORB;

		  }
	happyend:;

	  /* Output matters */

	/* transmit coordinates which were not changed, the rest overwrite below */
	Neutrons = InputNeutrons[i]; 



	/* translates neutron variables for output - X'= 0. . */

	{
		VectorType Path;

		Neutrons.Time = TOF + (- Pos[0]) / Dir[0] / V_FROM_LAMBDA(WL); 
			
		if(zerotime==1)
		{ 
			Neutrons.Time = fabs(fmod(Neutrons.Time + Phase/omega + coef_pi*M_PI/omega/2., coef_pi*M_PI/omega)) - coef_pi*M_PI/2./omega ;
		}

		CopyVector(Dir, Path);

		MultiplyByScalar(Path, (- Pos[0])/ Dir[0] );

		AddVector(Pos, Path);  /*Path = displacement vector */
		
		CopyVector(Pos, Neutrons.Position);
	}												 

	}
#endif

#if defined VITESS 

	  /* writes output binary file */
	  WriteNeutron(&Neutrons);

	}
}

  /* Do the general cleanup */

my_exit:;

  ChopperFermiCleanup();

  fprintf(LogFilePtr," \n");

  Cleanup(pos_ch[0],pos_ch[1],pos_ch[2], 0.0,0.0);	

  return 0;
}
#endif



#if defined VITESS || defined MCSTAS_SHARE

/* own initialization of the module */

void ChopperFermiInit(int argc, char *argv[])
{
  /*    INPUT  */

 #ifdef VITESS
  GeomFileName="chopper_fermi_g.dat";

  CurvGeomOption = 1;
 #endif

  while(argc>1)
    {
      char *arg = &argv[1][2];

      switch(argv[1][1])
	{
	case 'X':
	  sscanf(arg, "%lf", &pos_ch[0]);
	  break;
	
	case 'Y':
	  sscanf(arg, "%lf", &pos_ch[1]);
	  break;

	case 'V':
	  sscanf(arg, "%lf", &pos_ch[2]);
	  break;

	case 'a':
	  sscanf(arg, "%lf", &height);
	  break;

	case 'b':
	  sscanf(arg, "%lf", &width);
	  break;

	case 'c':
	  sscanf(arg, "%lf", &depth); 
	  break;

	case 'L':
	  sscanf(arg, "%lf", &optimal_wl);
	  break;

	case 'l':
	  sscanf(arg, "%ld", &Nchannels);
	  break;

	case 'm':
	  sscanf(arg, "%lf", &wallwidth);
	  break;

	case 'n':
	  sscanf(arg, "%lf", &omega);
	  if(omega == 0) omega = 1.E-6;
	  omega = omega * 2. * M_PI / 1000.;
	  break;

	case 'q':
	  sscanf(arg, "%lf", &Phase);
	  Phase = Phase * M_PI / 180.;
	  break;

	case 'O':
	  sscanf(arg, "%d", &Option);
	  if((Option!=1)&&(Option!=2)){fprintf(LogFilePtr,"\nERROR: Wrong option! Good options: 1-straight, 2-curved \n"); exit(-1); }
	  break;

	case 'g':
	  sscanf(arg, "%d", &CurvGeomOption);
	  break;

	case 'r':
	  sscanf(arg, "%lf", &diameter);
	  break;

	case 'G':
	  GeomFileName=arg;
	  break;

	case 'z':
	  sscanf(arg, "%d", &zerotime);
	  break;


	}
      argc--;
      argv++;
    }

	if(pos_ch[0] < diameter/2.) {fprintf(LogFilePtr,"\nERROR: Minimum position is diameter/2 \n"); exit(-1); }

	if(Nchannels==1) wallwidth=0.;

	
  /* calculate edge positions */ 
  
  if(Option==1)
    {

      fprintf(LogFilePtr,"\nStraight Fermi chopper option activated\n");

      /* diameter matter */

      main_depth = 2. * sqrt(sq(diameter/2.) - sq(width/2.));
      if(depth > main_depth) {
	fprintf(LogFilePtr,"\nERROR: Diameter too small - not compatible with 'channel length' and 'width'!\nTake min %f cm\n", 2. * sqrt(sq(depth/2.) + sq(width/2.)));

	exit(-1);
      }

	{
	  double add, w_ch;

	  long j, k, m;

	  if((     w_ch = ( width - (Nchannels + 1) * wallwidth ) / Nchannels    ) <= 0.)
	    {fprintf(LogFilePtr,"\nERROR: Channel width =< 0 !\n"); exit(-1);}

		fprintf(LogFilePtr,"Channel width: %f cm\n",w_ch);

	  for(k=0;k<4; k++) y_ch[k][0] =  - width/2.;

	  m = 1;

	  for(j=1; j< 2*Nchannels+2; j++)
	    {

	      if(m == 1) add = wallwidth; else add = w_ch;

	      for(k=0;k<4; k++) 
		  {
				x_ch[k][j] = - depth/2. * (3.- k)/3. + k/3. * depth/2.;

				y_ch[k][j] = y_ch[k][j-1] + add;
		  }

	      m = m * (-1);
	    }

	  for(k=0;k<4; k++) 
	  {		  
		x_ch[k][0] = x_ch[k][1] = x_ch[k][2*Nchannels] = x_ch[k][2*Nchannels+1] = - depth/2. *(3. - k)/3. + k/3. * depth/2.;
	  }

	  /* activating shadowing cylinder */

		x_ch[0][0] = x_ch[0][1] = x_ch[0][2*Nchannels] = x_ch[0][2*Nchannels+1] = - main_depth/2.;
	
		x_ch[3][0] = x_ch[3][1] = x_ch[3][2*Nchannels] = x_ch[3][2*Nchannels+1] = main_depth/2.;
	
	}

    }

  if(Option==2)
    {	
		  if(CurvGeomOption==1)
		  {
						  fprintf(LogFilePtr,"\nCurved Fermi chopper activated \n");

						  fprintf(LogFilePtr,"\nGeometry option: ideally shaped (close to parabolic) long channels ('channel length' inactive parameter) \n");

						  fprintf(LogFilePtr,"\nRadius of curvature (parabolic approximation):\n" 
											 "	%f cm at center\n" 
											 "	%f cm at circumference\n", 
											 V_FROM_LAMBDA(optimal_wl)/2./omega, V_FROM_LAMBDA(optimal_wl)/2./omega*pow((1 + sq(omega*diameter/V_FROM_LAMBDA(optimal_wl))), 1.5));


				  GeomFilePtr = fopen(GeomFileName,"w");
				  {
						double add, w_ch, xx[500], yy[500], tt;

						long j, m, k;

						if((     w_ch = ( width - (Nchannels + 1) * wallwidth ) / Nchannels    ) <= 0.)
						  {fprintf(LogFilePtr,"\nChannel width =< 0 !\n"); exit(-1);}

							fprintf(LogFilePtr,"Channel width: %f cm\n",w_ch);

							y_ch[0][0] =  - width/2.;
							x_ch[0][0] = sqrt(sq(diameter/2.) - sq(y_ch[0][0]));
							x_ch[3][0] = diameter/2. * sin(omega*diameter/V_FROM_LAMBDA(optimal_wl) + acos(2.*y_ch[0][0]/diameter));
							y_ch[3][0] = diameter/2. * cos(omega*diameter/V_FROM_LAMBDA(optimal_wl) + acos(2.*y_ch[0][0]/diameter));

						for(k=0; k<500; k++)
						  { tt = k /500. * 2 * M_PI; xx[k] = diameter/2. * cos(tt); yy[k] = diameter/2. * sin(tt); /* the circle */
						  fprintf(GeomFilePtr, " %ld %lf  %lf\n", 0, xx[k], yy[k]);
						  }

						m = 1;

						for(j=1; j< 2*Nchannels+2; j++)
						{

							if(m == 1) add = wallwidth; else add = w_ch;
							
							y_ch[0][j] = y_ch[0][j-1] + add;

							x_ch[0][j] = - sqrt(sq(diameter/2.) - sq(y_ch[0][j]));

							
							for(k=0; k<500; k++)
							  {
								tt= k /500. * 2. * fabs(x_ch[0][j])/V_FROM_LAMBDA(optimal_wl);
								xx[k] = y_ch[0][j] * sin(omega*tt) + (V_FROM_LAMBDA(optimal_wl)*tt - fabs(x_ch[0][j])) * cos(omega*tt);
								yy[k] = y_ch[0][j] * cos(omega*tt) - (V_FROM_LAMBDA(optimal_wl)*tt - fabs(x_ch[0][j])) * sin(omega*tt);

								fprintf(GeomFilePtr, "%ld   %lf  %lf\n", j, xx[k], yy[k]);		
							  }

								x_ch[1][j] = xx[167];
								y_ch[1][j] = yy[167];
								x_ch[2][j] = xx[333];
								y_ch[2][j] = yy[333];
								x_ch[3][j] = diameter/2. * sin(2.* omega*fabs(x_ch[0][j])/V_FROM_LAMBDA(optimal_wl) + acos(2.*y_ch[0][j]/diameter));
								y_ch[3][j] = diameter/2. * cos(2.* omega*fabs(x_ch[0][j])/V_FROM_LAMBDA(optimal_wl) + acos(2.*y_ch[0][j]/diameter));

								m = m * (-1);
						}
				  }
		  }
		if(CurvGeomOption==2)
		{

		  
					/* diameter matter */

				  main_depth = 2. * sqrt(sq(diameter/2.) - sq(width/2.));
				  if(depth > main_depth) {
					fprintf(LogFilePtr,"\nERROR: Diameter too small - not compatible with 'channel length' and 'width'!\nTake min %f cm\n", 2. * sqrt(sq(depth/2.) + sq(width/2.))); exit(-1);}


		  
				  fprintf(LogFilePtr,"\nCurved Fermi chopper activated \n");

				  fprintf(LogFilePtr,"\nGeometry option: circular shaped channels with fixed length (via 'channel length') \n");

				  radius_of_curv = V_FROM_LAMBDA(optimal_wl)/2./omega;

				  angle_channel = atan(depth/2./radius_of_curv);

				  fprintf(LogFilePtr,"\nRadius of curvature (parabolic approximation):\n" 
									 "optimal_velocity/2./omega = %f cm \n" 
									 "Angle channel: %f deg \n", 
									 radius_of_curv, 180./M_PI*angle_channel);


			  GeomFilePtr = fopen(GeomFileName,"w");
			  {
					double add, w_ch, xx[500], yy[500], tt;

					long j, m, k;

					if((     w_ch = ( width - (Nchannels + 1) * wallwidth ) / Nchannels    ) <= 0.)
					  {fprintf(LogFilePtr,"\nChannel width =< 0 !\n"); exit(-1);}

					fprintf(LogFilePtr,"Channel width: %f cm\n",w_ch);


					for(k=0; k<500; k++)
					  { tt = k /500. * 2 * M_PI; xx[k] = diameter/2. * cos(tt); yy[k] = diameter/2. * sin(tt); /* the circle */
					  fprintf(GeomFilePtr, " %ld %lf  %lf\n", -1, xx[k], yy[k]);
					  }

					m = 1;

					for(j=0; j< 2*Nchannels+2; j++)
					{

						if(m == 1) add = wallwidth; else add = w_ch;

								
						for(k=0; k<500; k++)
						  {
							xx[k]= (2.*k /499.  - 1.) * depth/2.;
							yy[k] = - width/2. - sqrt(sq(radius_of_curv) - sq(depth/2.)) + sqrt(sq(radius_of_curv) - sq(xx[k])) + shift_y;

							fprintf(GeomFilePtr, "%ld   %lf  %lf\n", j, xx[k], yy[k]);		
						  }

							x_ch[0][j] = xx[0];
							y_ch[0][j] = yy[0];
							x_ch[1][j] = xx[167];
							y_ch[1][j] = yy[167];
							x_ch[2][j] = xx[333];
							y_ch[2][j] = yy[333];
							x_ch[3][j] = xx[499];
							y_ch[3][j] = yy[499];

							shift_y += add;
							m = m * (-1);
					}
				  /* activating shadowing cylinder */

					x_ch[0][0] = x_ch[0][1] = x_ch[0][2*Nchannels] = x_ch[0][2*Nchannels+1] = - main_depth/2.;
				
					x_ch[3][0] = x_ch[3][1] = x_ch[3][2*Nchannels] = x_ch[3][2*Nchannels+1] = main_depth/2.;
	

			  }
		}
    }
		
  if(Option==1) coef_pi=1.; else coef_pi=2.;  fprintf(LogFilePtr,"Phase set is %f [deg].\n", 180./M_PI*fmod(Phase , coef_pi*M_PI));   

}/* End OwnInit */


/* own cleanup of the monochromator/analyser module */

void ChopperFermiCleanup()
{
  if (GeomFilePtr) fclose(GeomFilePtr);
}/* End OwnCleanup */

#endif
