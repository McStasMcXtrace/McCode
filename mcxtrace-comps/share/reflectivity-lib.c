#include <complex.h>
#include <stdarg.h>
#include <math.h>
#include <complex.h>

#ifdef REFLIBNAME
#undef REFLIBNAME
#endif
#define REFLIBNAME "reflectivity-lib"

int reflec_Init(t_Reflec *R, enum reflec_Type typ, ...){
  if (R==NULL){
    R=calloc(1,sizeof(t_Reflec));
  }
  va_list ap;
  int status;
  va_start(ap,typ);
  R->type=typ;
  switch(typ){
    case COATING_UNDEFINED:
      {
        /*no type is given: Assume that it is stated as a #param=<TYPE>
          item in the datafile header*/
        char *infile = va_arg(ap,char *);
        reflec_Init_File(R,infile);
        break;
      }
    case CONSTANT:
      R->prms.rconst.R=va_arg(ap,double);
      break;
    case BARE:
      R->prms.rb=va_arg(ap,struct t_reflec_bare);
      reflec_Init_File(R,R->prms.rc.matrl);
      break;
    case COATING:
      {
          /*cast the first address to be a scalar integer. The latter ones are pointers to double arrays.*/
          int N=(int)((double **) pars)[0];
          if(pars){
              reflec_Init_parratt(R, N,((double **) pars)[1], ((double **) pars)[2], ((double **) pars)[3]);        
          }else{
              fprintf(stderr,"WARNING:(%s) No parameters specified to Parratt reflectivity algortihm. Setting R=0.\n",REFLIBNAME);
              R->type=CONSTANT;
              R->rconst.R=0;
          }
          break;
      }
    case Q_PARAMETRIC:
      {
          if(pars){
              reflec_Init_kinematic(R, (int) pars[0],pars[1],pars[2],pars[3]);        
          }else{
              reflec_Init_kinematic(R, (int) 0, 0.0, 0.0, 0.0, 0.0);
          }
          break;
      }
    case PARRATT:
      {
        R->prms.rp.N = va_arg(ap,int);
        R->prms.rp.d = va_arg(ap,double *);
        R->prms.rp.delta = va_arg(ap,double *);
        R->prms.rp.beta = va_arg(ap,double *);
        break;
      }
    case KINEMATIC:
      {
        R->prms.rk.N = va_arg(ap,int);
        R->prms.rk.Gamma = va_arg(ap,double);
        R->prms.rk.Lambda = va_arg(ap,double);
        R->prms.rk.rho_AB = va_arg(ap,double);
        break;
      }
    case ETH_PARAMETRIC:
      {
        R->prms.rethpm.fname=va_arg(ap,char *);
        reflec_Init_File(R,R->prms.rethpm.fname);
/*        R->prms.rethpm.fname=va_arg(ap,char *);*/
/*          struct t_reflec_eth_prmtc *ptr=&(R->prms.rethpm);*/
/*          ptr->T=calloc(1,sizeof(t_Table));*/
/*          if ( (status=Table_Read(ptr->T,ptr->fname,0))==-1){*/
/*              fprintf(stderr,"Error (%s) Error: Could not parse file \"%s\"\n",__FILE__,ptr->fname);*/
/*              exit(-1);*/
/*          }*/
          /*parse header for E_min E_max etc.*/
/*          char **header_parsed;*/
/*          header_parsed = Table_ParseHeader(ptr->T->header,"e_min=","e_max=","theta_min=","theta_max=",NULL);*/
/*          if (header_parsed[0] && header_parsed[1] && header_parsed[2] && header_parsed[3]){*/
/*            ptr->emin=strtod(header_parsed[0],NULL);*/
/*            ptr->emax=strtod(header_parsed[1],NULL);*/
/*            ptr->thetamin=strtod(header_parsed[2],NULL);*/
/*            ptr->thetamax=strtod(header_parsed[3],NULL);*/
/*          }*/
/*          ptr->estep=(ptr->emax - ptr->emin)/(ptr->T->rows-1);*/
/*          ptr->thetastep=(ptr->thetamax - ptr->thetamin)/(ptr->T->columns-1);*/
          break;
      }
    default:
      fprintf(stderr,"Error (%s): Undetermined reflectivity parameterization type. r set to 1\n",REFLIBNAME);
      free(R);
      R=NULL;
      return 1;
  }
  va_end(ap);
  return 0;
}

int reflec_Init_parratt(t_Reflec *R, int N, double *d, double *delta, double *beta){
    R->rp.N=N;
    R->rp.d=d;
    R->rp.delta=delta;
    R->rp.beta=beta;
    return 0;
}

int reflec_Init_kinematic(t_Reflec *R, int N, double Gamma, double Lambda, double rhoAB){
    R->rp.N=N;
    R->rp.Gamma=Gamma;
    R->rp.Lambda=Lambda;
    R->rp.rhoAB=rhoAB;
    return 0;
}

/* Initialize a container object for various types of reflectivity parametrization using
 * an input file as source.*/

int reflec_Init_File(t_Reflec *R, char *filename){
    if (R==NULL){
      R=calloc(1,sizeof(t_Reflec));
    }
    /*a pointer representation has to be used here - otherwise the memory may be garbage collected
      upon return from this function.*/
    t_Table *table=malloc(sizeof(t_Table));

    /*if the filename is neither empty, blank, nor "NULL" read it, else return a constant opaque surface (R=0)*/
    int status;
    if(filename && strlen(filename) && strcmp(filename,"NULL") &&
        (status = Table_Read(table, filename, 1)!=-1) ) {
      R->type=get_table_reflec_type(table);
    }else{
      fprintf(stderr,"Warning: (%s) no reflectivity file given. Surface is opaque.\n","reflectivity-lib");
      R->type=CONSTANT;
      R->prms.rconst.R=0;
      return 0;
    }

    switch(R->type){
      case CONSTANT:
        {
          R->prms.rconst.R=Table_Index(*table, 0, 0);
          break;
        }

      case BARE:
        {
          char** header_parsed=Table_ParseHeader(table->header,"#material=", "#d=", NULL);
          if(!header_parsed[0] || !header_parsed[1] ){
            fprintf(stderr,"Error (%s) Error: Could not parse file \"%s\"\n",__FILE__,filename);
            exit(-1);
          }
          R->prms.rb.matrl=header_parsed[0];
          R->prms.rb.d=strtod(header_parsed[1], NULL);
          break;
        }

      case COATING:
        {
          char **header_parsed=Table_ParseHeader(table->header, "#material=", "#Z=", "#A[r]=", "#rho=", "#d=", NULL);
          if(!(header_parsed[0] && header_parsed[1] && header_parsed[2] && header_parsed[3] && header_parsed[4])){
            fprintf(stderr,"Error (%s) Error: Could not parse file \"%s\"\n",__FILE__,filename);
            exit(-1);
          } else {
            R->prms.rc.matrl=header_parsed[0];
            R->prms.rc.T = table;
            R->prms.rc.d = malloc(sizeof(double));
            *(R->prms.rc.d) = strtod(header_parsed[4], NULL);
            R->prms.rc.rho=strtod(header_parsed[3],NULL);
            R->prms.rc.Z=strtod(header_parsed[1],NULL);
            R->prms.rc.At=strtod(header_parsed[2],NULL);
          }
          break;
        }

      case Q_PARAMETRIC:
        {
          R->prms.rqpm.fname=filename;
          R->prms.rqpm.T=table;
          R->prms.rqpm.qmin=Table_Index(*table,0,0);
          R->prms.rqpm.qmax=Table_Index(*table,table->rows,0);
          break;
        }

      case PARRATT:
        {
          char **header_parsed = Table_ParseHeader(table->header, "#N=", "#d=", "#delta=", "#beta=", NULL);
          if (! (header_parsed[0] && header_parsed[1] && header_parsed[2] && header_parsed[3])){
            fprintf(stderr,"Error (%s) Error: Could not parse file \"%s\"\n",__FILE__,filename);
            exit(-1);
          }
          R->prms.rp.N = strtol(header_parsed[0], NULL, 10);
          R->prms.rp.d = malloc(sizeof(double));
          *(R->prms.rp.d) = strtod(header_parsed[1], NULL);
          R->prms.rp.delta = malloc(sizeof(double));
          *(R->prms.rp.delta) = strtod(header_parsed[2], NULL);
          R->prms.rp.beta = malloc(sizeof(double));
          *(R->prms.rp.beta) = strtod(header_parsed[3], NULL);
          break;
        }

      case KINEMATIC:
        {
          char **header_parsed = Table_ParseHeader(table->header, "#N=", "#gamma=", "#lambda=", "#rho_ab=", NULL);
          if (! (header_parsed[0] && header_parsed[1] && header_parsed[2] && header_parsed[3] && header_parsed[4])){
            fprintf(stderr,"Error (%s) Error: Could not parse file \"%s\"\n",__FILE__,filename);
            exit(-1);
          }

          R->prms.rk.N = strtol(header_parsed[0], NULL, 10);
          R->prms.rk.Gamma = strtod(header_parsed[2], NULL);
          R->prms.rk.Lambda = strtod(header_parsed[3], NULL);
          R->prms.rk.rho_AB = strtod(header_parsed[4], NULL);
          break;
        }

      case ETH_PARAMETRIC:
        {
          R->prms.rethpm.fname=filename;
          R->prms.rethpm.T=table;

          /*parse header for E_min E_max etc.*/
          char **header_parsed = Table_ParseHeader(table->header,"e_min=","e_max=","theta_min=","theta_max=",NULL);
          if (!(header_parsed[0] && header_parsed[1] && header_parsed[2] && header_parsed[3])){
            fprintf(stderr,"Error (%s) Error: Could not parse file \"%s\"\n",__FILE__,filename); //1619
            exit(-1);
          }
          R->prms.rethpm.emin=strtod(header_parsed[0],NULL);
          R->prms.rethpm.emax=strtod(header_parsed[1],NULL);
          R->prms.rethpm.thetamin=strtod(header_parsed[2],NULL);
          R->prms.rethpm.thetamax=strtod(header_parsed[3],NULL);
          int rows = R->prms.rethpm.T->rows;
          int cols = R->prms.rethpm.T->columns;
          if(rows == 0){ //implies cols == 0 as well
            fprintf(stderr,"Error (%s): File %s contains no table.",__FILE__, filename);
            exit(1);
          } else {
            if(rows == 1){
              printf("File %s contains only a single row. Setting e_step = 0", filename);
              R->prms.rethpm.estep=0;
            } else {
              R->prms.rethpm.estep=(R->prms.rethpm.emax - R->prms.rethpm.emin)/(rows-1);
            }
            if(cols == 1){
              printf("File %s contains only a single column. Setting theta_step = 0", filename);
              R->prms.rethpm.thetastep=0;
            } else {
              R->prms.rethpm.thetastep=(R->prms.rethpm.thetamax - R->prms.rethpm.thetamin)/(cols-1);
            }
          }
          break;
        }
      case UNDETERMINED:
      default:
        {
          fprintf(stderr,"Error (%s): Undetermined reflectivity parametrization type. r set to 1\n",REFLIBNAME);
          return 1;
        }
    }
    return 0;
}

enum reflec_Type get_table_reflec_type(t_Table *t){
    char **header_parsed = Table_ParseHeader(t->header,"param=",NULL);
    char *type = header_parsed[0];
    if(!type){
      printf("ERROR (%S):Couldn't read type from reflectivity table file.",REFLIBNAME);
        exit(-1);
    }
    /*for this to work well we need to trim the type string*/
    while (isspace(*type)){
      type++;
    }
    char *back = type+strlen(type);
    while (isspace(*(--back))){
      *(back++)='\0';
    }

    printf("INFO (%s)  reflectivity parameterization type: \'%s\'\n",REFLIBNAME, type);
    if(strcmp(type, NAME_CONSTANT) == 0){
        return CONSTANT;
    } else if(strcmp(type, NAME_BARE) == 0){
        return BARE;
    } else if(strcmp(type, NAME_COATING) == 0){
        return COATING;
    } else if(strcmp(type, NAME_Q_PARAMETRIC) == 0){
        return Q_PARAMETRIC;
    } else if(strcmp(type, NAME_PARRATT) == 0){
        return PARRATT;
    } else if(strcmp(type, NAME_ETH_PARAMETRIC) == 0){
        return ETH_PARAMETRIC;
    } else if(strcmp(type, NAME_KINEMATIC) == 0){
        return KINEMATIC;
    } else {
        printf("ERROR (%s): \'%s\'; is not a known parametrization!", REFLIBNAME, type);
        exit(-1);
    }
}

/*This section contains the functions that compute the actual reflectivity*/
double complex reflec_coating(t_Reflec *r_handle, double q, double g, double k){
    struct t_reflec_coating *ptr=&(r_handle->prms.rc);
    /*adjust p according to reflectivity*/
    double Qc,f1,f2,na,e;
    /*length of wavevector transfer may be compute from s and n_ above*/

    /*interpolate in material data*/
    e=K2E*k;
    f1=Table_Value(*(ptr->T),e,1);
    f2=Table_Value(*(ptr->T),e,2);
    /*the conversion factor in  the end is to transform the atomic density from cm^-3 to AA^-3
    -> therefore we get Q in AA^-1*/
    na=NA*ptr->rho/ptr->At*1e-24;
    Qc=4*sqrt(M_PI*na*RE*f1);

    double complex qp,qn,rr;
    double b_mu,R;

    qn=q/Qc;
    /*delta=na*r0*2*M_PI/k2*f1;*/
    /*beta=na*r0*2*M_PI/k^2*f2; b_mu=beta*(2*k)^2 / Qc^2*/
    b_mu=8*M_PI*na*RE*f2/(Qc*Qc);
    if(qn==1){
        qp=sqrt(b_mu)*(1+I);
    }else {
        qp=csqrt((qn*qn)-1+2*I*b_mu);
    }
    /*and from this compute the reflectivity*/
    rr=(qn-qp)/(qn+qp);
    return rr;
}

double complex reflec_bare(t_Reflec *r_handle, double q, double g){
    return 0.0;
}

double complex reflec_kinematic(t_Reflec *r_handle, double q, double g){
    double complex r1,rN;
    struct t_reflec_kinematic *ptr=&(r_handle->prms.rk);
    double zeta=ptr->Lambda*q/(2*M_PI);
    double beta=0;
    r1 = -2*I*RE* ptr->rho_AB * (pow(ptr->Lambda,2.0)*ptr->Gamma/zeta) * sin(M_PI*ptr->Gamma*zeta)/(M_PI*ptr->Gamma*zeta);
    rN = r1*(1-cexp(I*2*M_PI*zeta*ptr->N)*exp(-beta*ptr->N))/(1-cexp(I*2*M_PI*zeta)*exp(beta));
    return rN;
}

double complex reflec_q_prmtc(t_Reflec *r_handle, double q, double g){
    double r;
    struct t_reflec_q_prmtc *ptr=&(r_handle->prms.rqpm);
    if (ptr->T->columns>2){
        double c=(ptr->T->columns-2)*g+1;
        r=Table_Value2d(*(ptr->T),ptr->T->rows*q/(ptr->qmax-ptr->qmin),c);
    }else{
        r=Table_Value(*(ptr->T),q,1);
    }
    return (double complex)r;
}

double complex reflec_eth_prmtc(t_Reflec *r_handle, double g, double e, double th){
    double r,ec,thc;
    struct t_reflec_eth_prmtc *ptr=&(r_handle->prms.rethpm);
    ec=(ptr->T->rows-1) * (e-ptr->emin)/(ptr->emax - ptr->emin);
    thc=(ptr->T->columns-1)*(th-ptr->thetamin)/(ptr->thetamax - ptr->thetamin);
    r=Table_Value2d(*(ptr->T),ec,thc);
    return (double complex)r;
}

/* Entry function to Parratt's recursive algorithm for multilayers.*/
double complex reflec_parratt(t_Reflec *r_handle, double q, double g, double k){
    double complex r,qp,rp;
    double k2;
    double complex qinf;
    double qpd,rd,p;
    t_reflec_parratt *pp=&(r_handle->prms.rp);

    qp=q;
    qpd=csqrt(q*q - 8*k2* *(pp->delta) + I*8*k2* *(pp->beta));
    k2=k*k;

    if (pp->N>0){
        rp=(qp-qpd)/(qp+qpd);
        rd=parrat_reflec_bulk(pp->N,pp->delta,pp->beta,pp->d,k,q);
        p=cexp(I*qpd* *(pp->d));
        r = (rp+p*rd)/(1+rp*rd*p);
    }else{
        r = (qp-qpd)/(qp+qpd);
    }
    return r;
}
/* Lower layer function fo rParratt's recursive algorithm. Here recursion
* takes place by calls to itself.*/
double complex parrat_reflec_bulk(int N, double *delta, double *beta, double *d, double k, double q){
    double complex qp,rp,rr;
    double k2=k*k;
    double complex qinf;
    double complex qpd,rpd,p;

    qp=csqrt(q*q - 8*k2* *delta + I*8*k2* *beta);
    qpd=csqrt(q*q - 8*k2* *(delta+1) + I*8*k2* *(beta+1));

    if (N>1){
        rp=(qp-qpd)/(qp+qpd);
        rpd=parrat_reflec_bulk(N-1,(delta+1),(beta+1),(d+1),k,q);
        p=cexp(I*qpd* d[1]);
        rr= (rp+p*rpd)/(1+rp*rpd*p);
    }
    if (N==1){
        /*the bottom layer (on top of substrate)*/
        rr=(qp-qpd)/(qp+qpd);
    }
    return rr;
}

/* Dispatcher functions that call the underlying computations depending on the type of reflectivity*/

double complex refleccq( t_Reflec *r_handle, double q, double g, ...){
    double complex r;
    /*using the normalized coordinate g which lies along the grading direction*/
    va_list varg;
    va_start(varg,g);

    switch(r_handle->type){
      case CONSTANT:
        {
          r=r_handle->prms.rconst.R;
          break;
        }
      case BARE:
        {
          r=reflec_bare(r_handle,q,g);
          break;
        }
      case COATING:
        {
          double k=va_arg(varg,double);
          r=reflec_coating(r_handle,q,g,k);
          break;
        }
      case Q_PARAMETRIC:
        {
          r=reflec_q_prmtc(r_handle,q,g);
          break;
        }
      case PARRATT:
        {
          double k=va_arg(varg,double);
          r=reflec_parratt(r_handle,q,g,k);
          break;
        }
      case ETH_PARAMETRIC:
        {
          double e=va_arg(varg,double);
          double theta=va_arg(varg,double);
          r=reflec_eth_prmtc(r_handle,g,e,theta);
          break;
        }
      case KINEMATIC:
        {
          r=reflec_kinematic(r_handle,q,g);
          break;
        }
      default:
        {
          fprintf(stderr,"Error (%s): Undetermined reflectivity type. r set to 1\n", REFLIBNAME);
          return 1.0;
        }
    }
    va_end(varg);
    return r;
}

double reflecq( t_Reflec *r_handle, double q, double g, ...){
    double r;
    va_list varg;
    va_start(varg,g);

    switch(r_handle->type){
      case CONSTANT:
        {
          r=cabs(r_handle->prms.rconst.R);
          break;
        }
      case BARE:
        {
          r=cabs(reflec_bare(r_handle,q,g));
          break;
        }
      case COATING:
        {
            double k=va_arg(varg,double);
            double complex rp;
            rp=reflec_coating(r_handle,q,g,k);
            r= creal(rp * conj(rp));
            break;
        }
      case Q_PARAMETRIC:
        {
          r=cabs(reflec_q_prmtc(r_handle,q,g));
          break;
        }
      case PARRATT:
        {
          double k=va_arg(varg,double);
          r=cabs(reflec_parratt(r_handle,q,g,k));
          break;
        }
      case ETH_PARAMETRIC:
        {
            double k=va_arg(varg,double);
            double theta=va_arg(varg,double);
            r=cabs(reflec_eth_prmtc(r_handle,g,k*K2E,theta));
            break;
        }
      case KINEMATIC:
        {
          r=cabs(reflec_kinematic(r_handle,q,g));
          break;
        }
      default:
        {
          fprintf(stderr,"ERROR (%s): Undetermined reflectivity type. r set to 1\n", REFLIBNAME);
          return 1.0;
        }
    }
    va_end(varg);
    return r;
}

double refleceth( t_Reflec *r_handle,double e, double th, double g){
    double q;
    double k=e*E2K;
    q=k*2.0*sin(th);
    if(r_handle->type==PARRATT || r_handle->type==COATING){
        return reflecq(r_handle,q,g,k);
    }else{
        return reflecq(r_handle,q,g,e,th);
    }
}

double complex reflecceth( t_Reflec *r_handle,double e, double th, double g){
    double q;
    double k=e*E2K;
    q=k*2.0*sin(th);
    if(r_handle->type==PARRATT){
        return refleccq(r_handle,q,g,k);
    }else{
        return refleccq(r_handle,q,g,e,th);
    }
}
