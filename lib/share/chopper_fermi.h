
#define	STRING_BUFFER 100

/* other global parameters for chopper_fermi */
int        Option,            /* 0: straight FC,  1: curved FC */
           CurvGeomOption;    /* 1: ideal shape (nearly parabolic)  1: circular */
double     TOF,               /* TOF of neutron under consideration */
           TOF_zero, 
           WL,                /* wavelength of neutron              */
           radius_of_curv,    /* radius of curvature (curved FC)    */
           main_depth, 
           shift_y=0., 
           angle_channel, 
           phase0, 
           y_ch[10][2000], 
           x_ch[10][2000], 
           coef_pi;
char       XFILEName[STRING_BUFFER];
char*      GeomFileName=sGeomFileName;
FILE*      GeomFilePtr=NULL;  /* pointer to geometry file        */
VectorType Pos,               /* position of neutron             */
           Dir;               /* flight direction of neutron     */
Neutron    Neutrons;          /* parameter set at end of chopper */
