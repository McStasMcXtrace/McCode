#include <complex.h>
#include <stdarg.h>
#include <math.h>
#include <complex.h>

int reflec_Init(t_Reflec *R, enum reflec_Type typ, ...){
  if (R==NULL){
    R=calloc(1,sizeof(t_Reflec));
  }
  va_list ap;
  int status;
  va_start(ap,typ);
  R->type=typ;
  switch(typ){
    case CONSTANT:
      R->prms.rconst.R=va_arg(ap,double);
      break;
    case BARE:
      R->prms.rb=va_arg(ap,struct t_reflec_bare);
      break;
    case COATING:
      R->prms.rc=va_arg(ap,struct t_reflec_coating);
      break;
    case Q_PARAMETRIC:
      {
          R->prms.rqpm.fname=va_arg(ap,char *);
          struct t_reflec_q_prmtc *ptr=&(R->prms.rqpm);
          ptr->T=calloc(1,sizeof(t_Table));
          if ( (status=Table_Read(ptr->T,ptr->fname,0))==-1){
              fprintf(stderr,"Error (%s) Error: Could not parse file \"%s\"\n",__FILE__,ptr->fname);
              exit(-1);
          }
          ptr->qmin=Table_Index(*(ptr->T),0,0);
          ptr->qmax=Table_Index(*(ptr->T),ptr->T->rows,0);
          break;
      }
    case PARRATT:
      R->prms.rp=va_arg(ap,struct t_reflec_parratt);
      break;
    case KINEMATIC:
      R->prms.rk=va_arg(ap,struct t_reflec_kinematic);
      break;
    case ETH_PARAMETRIC:
      {
          R->prms.rethpm.fname=va_arg(ap,char *);
          struct t_reflec_eth_prmtc *ptr=&(R->prms.rethpm);
          ptr->T=calloc(1,sizeof(t_Table));
          if ( (status=Table_Read(ptr->T,ptr->fname,0))==-1){
              fprintf(stderr,"Error (%s) Error: Could not parse file \"%s\"\n",__FILE__,ptr->fname);
              exit(-1);
          }
          /*parse header for E_min E_max etc.*/
          char **header_parsed;
          header_parsed = Table_ParseHeader(ptr->T->header,"e_min=","e_max=","theta_min=","theta_max=",NULL);
          if (header_parsed[0] && header_parsed[1] && header_parsed[2] && header_parsed[3]){
            ptr->emin=strtod(header_parsed[0],NULL);
            ptr->emax=strtod(header_parsed[1],NULL);
            ptr->thetamin=strtod(header_parsed[2],NULL);
            ptr->thetamax=strtod(header_parsed[3],NULL);
          }
          ptr->estep=(ptr->emax - ptr->emin)/(ptr->T->rows-1);
          ptr->thetastep=(ptr->thetamax - ptr->thetamin)/(ptr->T->columns-1);
          break;
      }
    default:
      fprintf(stderr,"Error (%s): Undetermined reflectivity type. r set to 1\n",__FILE__);
      free(R);
      R=NULL;
      return 1;
  }
  va_end(ap);
  return 0;
}


double complex reflec_coating(t_Reflec *r_handle, double q, double g){
  return 0.0;
}

double complex reflec_bare(t_Reflec *r_handle, double q, double g){
  return 0.0;
}

double complex reflec_kinematic(t_Reflec *r_handle, double q, double g){
  double complex r;
  struct t_reflec_kinematic *ptr=&(r_handle->prms.rk);
  r=2*I*RE* ptr->rho_AB * (pow(ptr->Gamma,2.0)*ptr->Lambda/ptr->zeta) * sin(M_PI*ptr->Lambda*ptr->zeta)/(ptr->Lambda*ptr->zeta);
  return r;
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

double complex reflec_parratt(t_Reflec *r_handle, double q, double g, double k){
  double complex r,qp,rp;
  double k2;
  double complex qinf;
  double qpd,rd,p;
  t_reflec_parratt *pp=&(r_handle->prms.rp);

  qp=q;
  qpd=csqrt(q*q - 8*k2* *(pp->delta) + I*8*k2* *(pp->beta));
  k2=k*k;//scalar_prod(mcnlkx,ky,kz,kx,ky,kz);
  //k=sqrt(k2);

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

/*double complex reflecc(t_Reflec *r_handle, double kix, double kiy, double kiz, double kfx, double kfy, double kfz, double g){*/
/*    double complex r;*/
/*    double q,qx,qy,qz;*/
/*    double e,theta;*/
/**/
/*    qx=kfx-kix;qy=kfy-kiy; qz=kfz-kiz;*/
/*    q=sqrt(scalar_prod(qx,qy,qz,qx,qy,qz));*/
    /*using the normalized coordinate g which lies along the grading direction*/
/*    switch(r_handle->type){*/
/*        case CONSTANT:*/
/*            r=r_handle->prms.rconst.R;*/
/*            break;*/
/*        case BARE:*/
/*            r=reflec_bare(r_handle,q,g);*/
/*            break;*/
/*        case COATING:*/
/*            r=reflec_coating(r_handle,q,g);*/
/*            break;*/
/*        case Q_PARAMETRIC:*/
/*            r=reflec_q_prmtc(r_handle,q,g);*/
/*            break;*/
/*        case PARRATT:*/
/*            {*/
/*                double k=sqrt(kix*kix + kiy*kiy + kiz*kiz)*K2E;*/
/*                r=reflec_parratt(r_handle,q,g,k);*/
/*                break;*/
/*            }*/
/*        case ETH_PARAMETRIC:*/
/*            {*/
/*                double k=sqrt(kix*kix + kiy*kiy + kiz*kiz)*K2E;*/
/*                double e=k*E2K;*/
/*                theta=acos(scalar_prod(kix,kiy,kiz,kfx,kfy,kfz))/2.0;*/
/*                r=reflec_eth_prmtc(r_handle,e,theta,g);*/
/*                break;*/
/*            }*/
/*        case KINEMATIC:*/
/*            r=reflec_kinematic(r_handle,q,g);*/
/*            break;  */
/*        default:*/
/*            fprintf(stderr,"Error (reflectivity-lib): Undetermined reflectivity type. r set to 1\n");*/
/*            return 1.0;*/
/*    }*/
/*    return r;*/
/*}*/


double complex refleccq( t_Reflec *r_handle, double q, double g, ...){
  double complex r;
  /*using the normalized coordinate g which lies along the grading direction*/
  va_list varg;
  va_start(varg,g);

  switch(r_handle->type){
    case CONSTANT:
      r=r_handle->prms.rconst.R;
      break;
    case BARE:
      r=reflec_bare(r_handle,q,g);
      break;
    case COATING:
      r=reflec_coating(r_handle,q,g);
      break;
    case Q_PARAMETRIC:
      r=reflec_q_prmtc(r_handle,q,g);
      break;
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
      r=reflec_kinematic(r_handle,q,g);
      break;  
    default:
      fprintf(stderr,"Error (reflectivity-lib): Undetermined reflectivity type. r set to 1\n");
      return 1.0;
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
            r=cabs(r_handle->prms.rconst.R);
            break;
        case BARE:
            r=cabs(reflec_bare(r_handle,q,g));
            break;
        case COATING:
            r=cabs(reflec_coating(r_handle,q,g));
            break;
        case Q_PARAMETRIC:
            r=cabs(reflec_q_prmtc(r_handle,q,g));
            break;
        case PARRATT:
            {
                double k=va_arg(varg,double);
                r=cabs(reflec_parratt(r_handle,q,g,k));
                break;
            }
        case ETH_PARAMETRIC:
            {
                double e=va_arg(varg,double);
                double theta=va_arg(varg,double);
                r=cabs(reflec_eth_prmtc(r_handle,g,e,theta));
                break;
            }
        case KINEMATIC:
            r=cabs(reflec_kinematic(r_handle,q,g));
            break;  
        default:
            fprintf(stderr,"Error (reflectivity-lib): Undetermined reflectivity type. r set to 1\n");
            return 1.0;
    }
    va_end(varg);
    return r;
}

double complex parrat_reflec(int lc, double *delta, double *beta, double *d, double k, double q){
  double complex qp,rp,rr;
  double k2=k*k;
  double complex qinf;
  double qpd,rd,p; 
  qp=q;
  qpd=csqrt(q*q - 8*k2* *delta + I*8*k2* *beta);
  if (lc>0){
    rp=(qp-qpd)/(qp+qpd);
    //rd=parrat_reflec_bulk(lc,delta,beta,d,k,q,0);
    p=cexp(I*qpd* *d);
    rr= (rp+p*rd)/(1+rp*rd*p);
  }else{
    return (qp-qpd)/(qp+qpd);
  }
  return rr; 
}

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

