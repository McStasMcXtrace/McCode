
#define	STRING_BUFFER 100

/* other global parameters for chopper_fermi */
int        Option,            /* 1: straight FC,  2: curved FC                       */
           CurvGeomOption;    /* 1: ideal shape (nearly parabolic)  2: circular      */
double     TOF,               /* TOF of neutron under consideration                  */
           WL,                /* wavelength of neutron                               */
           radius_of_curv,    /* radius of curvature (curved FC)                     */
           main_depth,        /* max. channel length due to diameter and total_width */
           shift_y=0.,        /* shift to channel actually written to geometry file  */ 
           angle_channel,     /* half of the curvature of a curved Fermi chopper     */
           phase0,            /* chopper phase at TOF of neutron to chopper centre   */
           y_ch[10][2000],    /* position of gates perpendicular to flight direction */
           x_ch[10][2000],    /* position of gates along flight direction            */
           coef_pi;           /* number of half-rotation to reach identical state    */
char*      GeomFileName=sGeomFileName; /* pointer to geometry file name */
FILE*      GeomFilePtr=NULL;  /* pointer to geometry file        */
VectorType Pos,               /* position of neutron             */
           Dir;               /* flight direction of neutron     */
Neutron    Neutrons;          /* parameter set at end of chopper */
