/*A library for computing reflectivity in surfaces with multilayers etc.*/
#include <complex.h>
#include <math.h>
#include <stdarg.h>
double Table_Value2d(t_Table, double, double);

enum reflec_Type  {CONSTANT=0,BARE, COATING, Q_PARAMETRIC, PARRATT, ETH_PARAMETRIC, KINEMATIC};

typedef struct t_reflec_constant{
  double R;
} t_reflec_constant;
 
typedef struct t_reflec_bare{
  char *matrl;
  double d;
} t_reflec_bare;

typedef struct t_reflec_coating{
  char *matrl;
  t_Table *T;
  double *d;
  double rho,Z,At;
}t_reflec_coating;

typedef struct t_reflec_q_prmtc{
  char *fname;
  t_Table *T;
  double qmin,qmax;
} t_reflec_q_prmtc;

typedef struct t_reflec_parratt{
  int N;
  double *d, *delta, *beta;
} t_reflec_parratt;

/**Multilayer in the kinematical approximation*/
typedef struct t_reflec_kinematic{
  int N;/**The number of bilayers in the multilayer*/
  double zeta;/**The thickness of the bilayer*/
  double Gamma, Lambda;/** */ 
  double rho_AB;/**Electron density contrast between material A and B*/
} t_reflec_kinematic;

typedef struct t_reflec_eth_prmtc{
  char *fname;
  t_Table *T;
  double emin,emax,estep;/*energy range*/
  double thetamin,thetamax,thetastep;/*incidence angle range*/
} t_reflec_theta_e_prmtc;

typedef struct reflec_T {
  enum reflec_Type type; 
  union {
    struct t_reflec_bare rb;
    struct t_reflec_coating rc;
    struct t_reflec_q_prmtc rqpm;
    struct t_reflec_parratt rp;
    struct t_reflec_constant rconst;
    struct t_reflec_kinematic rk;
    struct t_reflec_eth_prmtc rethpm;
  } prms;
} t_Reflec;

/* reflectivity-lib.c */
int reflec_Init(t_Reflec *R, enum reflec_Type typ, ...);

double complex refleccq(t_Reflec *r_handle, double q, double g, ... );
double reflecq(t_Reflec *r_handle, double q, double g, ... );
double complex reflecc(t_Reflec *r_handle, double kix, double kiy, double kiz, double kfx, double kfy, double kfz, double g );

double complex reflec_coating(t_Reflec *r_handle, double q, double g, double k);
double complex reflec_bare(t_Reflec *r_handle, double q, double g);
double complex reflec_q_prmtc(t_Reflec *r_handle, double q, double g);
double complex reflec_parratt(t_Reflec *r_handle, double q, double g, double k);
double complex reflec_kinematical(t_Reflec *r_handle, double q, double g);
double complex parrat_reflec_bulk(int lc, double *delta, double *beta, double *d, double k, double q);
double complex reflec_eth_prmtc(t_Reflec *r_handle, double e, double theta, double g);
