
#include "nxs.h"

#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>


#ifndef M_PI
  #define M_PI 3.14159265358979323846
#endif


#define array_length(array) (sizeof(array)/sizeof(array[0]))


/* GLOBAL CONSTANTS */
double BOLTZMANN_CONSTANT_evK = 8.617343E-5; // in [eV/K]
double BOLTZMANN_CONSTANT_JK = 1.3806504E-23; // in [J/K]
double PLANCK_CONSTANT_eVs = 4.13566733E-15; // in [eVs]
double PLANCK_CONSTANT_Js = 6.62606896E-34; // in [Js]
double PLANCK_BAR_eVs = 6.58211899E-16; // in [eVs] (h_ = h/2pi)
double PLANCK_BAR_Js = 1.054571628E-34; // in [Js]
double MASS_NEUTRON_kg = 1.6749E-27; // [kg]
double EV2J = 1.6021773E-19; // 1eV = 1.6021773E-19J
double J2EV = 6.2415063E18;
double AVOGADRO = 6.0221417930E23; // [mol-1]
double ATOMIC_MASS_U_kg = 1.6605402E-27; // atomic_mass in kg




int dhkl_compare( const void *par1, const void *par2)
{
  return ( ((NXS_HKL*)par1)->dhkl < ((NXS_HKL*)par2)->dhkl ) ? 1 : 0;
}



int equivhkl_compare( const void *par1, const void *par2)
{
  int hkl1, hkl2;
  hkl1 = abs( (int)1E4*((NXS_EquivHKL*)par1)->h ) + abs( (int)1E2*((NXS_EquivHKL*)par1)->k ) + abs( ((NXS_EquivHKL*)par1)->l );
  hkl2 = abs( (int)1E4*((NXS_EquivHKL*)par2)->h ) + abs( (int)1E2*((NXS_EquivHKL*)par2)->k ) + abs( ((NXS_EquivHKL*)par2)->l );
  return ( (hkl1<hkl2)  ? 1 : 0 );
}



double distance( double x1, double y1, double z1, double x2, double y2, double z2 )
{
  double dx = x2 - x1;
  double dy = y2 - y1;
  double dz = z2 - z1;

  return sqrt( dx*dx + dy*dy + dz*dz );
}


/**
 * \fn int generateWyckoffPositions( NXS_UnitCell *uc )
 * Using SgInfo library function this initialises the unit cell (including its volume).
 * @param us NXS_UnitCell struct
 * @return
 */
int generateWyckoffPositions( NXS_UnitCell *uc )
{
  unsigned int nTrV,  nLoopInv, i,j,iLoopInv,iAtomInfo;
  const T_RTMx *lsmx;
  T_RTMx SMx;
  int iList;
  T_SgInfo SgInfo;

  SgInfo = uc->sgInfo;

  nTrV = SgInfo.LatticeInfo->nTrVector;

  nLoopInv = Sg_nLoopInv(&SgInfo);

  uc->nAtoms = 0;

  for( iAtomInfo=0; iAtomInfo<uc->nAtomInfo; iAtomInfo++ )
  {
    NXS_AtomInfo ai;
    double x0, y0, z0;
    int *TrV;

    ai = uc->atomInfoList[iAtomInfo];
    ai.nAtoms = 0;
    x0 = ai.x[0];
    y0 = ai.y[0];
    z0 = ai.z[0];

    TrV = SgInfo.LatticeInfo->TrVector;
    for( j=0; j<nTrV; j++, TrV+=3 )
    {
      for( iLoopInv=0; iLoopInv<nLoopInv; iLoopInv++ )
      {
        int f = -1;
        if( iLoopInv==0 ) f = 1;

        lsmx = SgInfo.ListSeitzMx;
        for( iList=0; iList<SgInfo.nList; iList++, lsmx++ )
        {
          double x,y,z;
          unsigned int l;
          int isExclusive;
          for( i=0; i<9; i++ )
            SMx.s.R[i] = f * lsmx->s.R[i];
          for( i=0; i<3; i++ )
            SMx.s.T[i] = iModPositive(f * lsmx->s.T[i] + TrV[i], STBF);

          x = (double)SMx.s.R[0]*x0 + (double)SMx.s.R[1]*y0 + (double)SMx.s.R[2]*z0 + (double)SMx.s.T[0]/(double)STBF;
          y = (double)SMx.s.R[3]*x0 + (double)SMx.s.R[4]*y0 + (double)SMx.s.R[5]*z0 + (double)SMx.s.T[1]/(double)STBF;
          z = (double)SMx.s.R[6]*x0 + (double)SMx.s.R[7]*y0 + (double)SMx.s.R[8]*z0 + (double)SMx.s.T[2]/(double)STBF;

          x = x<0.0 ? fmod(1.0-fmod(-x,1.0),1.0) : fmod(x,1.0);
          y = y<0.0 ? fmod(1.0-fmod(-y,1.0),1.0) : fmod(y,1.0);
          z = z<0.0 ? fmod(1.0-fmod(-z,1.0),1.0) : fmod(z,1.0);

          l = 0;
          isExclusive = 1;
          while( l<ai.nAtoms && isExclusive )
          {
            if( distance(x,y,z,ai.x[l],ai.y[l],ai.z[l]) < 1E-3 )
              isExclusive = 0;
            else
              l++;
          }

          if( isExclusive )
          {
            ai.x[ai.nAtoms] = x;
            ai.y[ai.nAtoms] = y;
            ai.z[ai.nAtoms] = z;
            ai.nAtoms++;
          }
        }
      }
    }

    uc->nAtoms += ai.nAtoms;
    uc->atomInfoList[iAtomInfo] = ai;
  }
  return 0;
}




/**
 * \fn calcFSquare( NXS_HKL *hklReflex, NXS_UnitCell* uc )
 * This function calculates the structure factor squared \f$|F|^2\f$.
 *
 * @param hklReflex NXS_HKL struct
 * @param uc NXS_UnitCell struct
 * @return
 */
double calcFSquare( NXS_HKL *hklReflex, NXS_UnitCell *uc )
{
  unsigned int i, j;
  int h = hklReflex->h;
  int k = hklReflex->k;
  int l = hklReflex->l;
  double dhkl = hklReflex->dhkl;

  double structure_factor_square = 0.0;
  double real = 0.0;
  double imag = 0.0;

  for( i=0; i<uc->nAtomInfo; i++ )
  {
//     double bcoh_100 = sqrt( uc->atomInfoList[i].sigmaCoherent/4.0/M_PI );
    double exp_B_iso = exp( -uc->atomInfoList[i].B_iso/4.0/dhkl/dhkl );
    double sin_exp = 0.0;
    double cos_exp = 0.0;

    for( j=0; j<uc->atomInfoList[i].nAtoms; j++ )
    {
      double x = uc->atomInfoList[i].x[j];
      double y = uc->atomInfoList[i].y[j];
      double z = uc->atomInfoList[i].z[j];

      if( fabs(x+y+z)<1E-6 )
        cos_exp += 1.0;
      else
      {
        double exponent = 2.0*M_PI*(x*h+y*k+z*l);
        sin_exp += sin( exponent );
        cos_exp += cos( exponent );
      }
    }
    sin_exp *= exp_B_iso * uc->atomInfoList[i].b_coherent;
    cos_exp *= exp_B_iso * uc->atomInfoList[i].b_coherent;
    real += cos_exp;
    imag += sin_exp;
  }
  structure_factor_square = (real*real + imag*imag);

  return structure_factor_square;
}




/**
 * \fn double calcDhkl( int h, int k, int l, NXS_UnitCell *uc )
 * This function calculates the lattice spacing in &Aring; depending on the hkl Miller indices and the crystal system.
 *
 * @param h Miller index h
 * @param k Miller index k
 * @param l Miller index l
 * @param uc NXS_UnitCell struct
 * @return
 */
double calcDhkl( int h, int k, int l, NXS_UnitCell *uc )
{
  double d_spacing = 0.0;
  double a = uc->a;
  double b = uc->b;
  double c = uc->c;
  double alpha = uc->alpha;
  double beta = uc->beta;
  double gamma = uc->gamma;
  double t1, t2, t3, t4, t5, t6, volume;

  //   h = abs(h);
  //   k = abs(k);
  //   l = abs(l);

  switch( uc->crystalSystem )
  {
    /* XS_Cubic */
    case 7:   d_spacing = a / sqrt( (double)((h*h) + (k*k) + (l*l)) );
              break;
    /* XS_Hexagonal */
    case 6:   d_spacing = a / sqrt( 4.0/3.0*(h*h + h*k + k*k) + (a*a/(c*c) * l*l) );
              break;
    /* XS_Trigonal */
    case 5:   d_spacing = sqrt(3.0)*a*c / sqrt( 4.0*(h*h+k*k+h*k)*c*c + 3.0*l*l*a*a );
              break;
    /* XS_Tetragonal */
    case 4:   d_spacing = a / sqrt( h*h + k*k + a*a/(c*c)*l*l );
              break;
    /* XS_Orthorhombic */
    case 3:   d_spacing = 1.0 / sqrt( h*h/a/a + k*k/b/b + l*l/c/c );
              break;
    /* XS_Monoclinic */
    case 2:   d_spacing =  a*b*c*sqrt( 1.0-cos(beta)*cos(beta) ) / sqrt( b*b*c*c*h*h + a*a*c*c*k*k*sin(beta)*sin(beta) + a*a*b*b*l*l - 2.0*a*b*b*c*h*l*cos(beta) );
              break;
    /* XS_Triclinic */
    case 1:   t1 = b*b*c*c * sin(alpha);
              t2 = a*a*c*c * sin(beta);
              t3 = a*a*b*b * sin(gamma);
              t4 = a*b*c*c * ( cos(alpha)*cos(beta)-cos(gamma) );
              t5 = a*a*b*c * ( cos(beta)*cos(gamma)-cos(alpha) );
              t6 = b*b*c*c * ( cos(gamma)*cos(alpha)-cos(beta) );
              volume = a*b*c * sqrt( 1.0 - cos(alpha)*cos(alpha) - cos(beta)*cos(beta) - cos(gamma)*cos(gamma) + 2.0*cos(alpha)*cos(beta)*cos(gamma) );

              d_spacing = 1.0/volume/volume * ( t1*h*h + t2*k*k + t3*l*l + 2.0*t4*h*k + 2.0*t5*k*l + 2.0*t6*h*l );
              d_spacing = sqrt( 1.0 / d_spacing );
              break;
    /* XS_Unknown */
    case 0:   break;
    default:  break;
  }

  return d_spacing;
}




double calcPhi_1( double theta )
{
  double phi_1, a_n, step1, step2, n, riemann_zeta_2, I_m;

  step1 = 1.0;
  step2 = 0.0;
  n = 1.0;
  a_n = 0.0;
  while ( fabs(step1-step2)>1E-6 )
  {
    step1 = step2;
    step2 = 1.0/( exp(n/theta)*n*n );

    a_n += step2;
    n = n + 1.0;
  }

  riemann_zeta_2 = M_PI*M_PI/6.0;
  I_m = theta * log( 1.0-exp(-1.0/theta) ) + theta*theta * (riemann_zeta_2 - a_n);

  phi_1 = 0.5 + 2.0*I_m;

  return phi_1;
}




double calcPhi_3( double theta )
{
  double phi_3;

  double a_n, step1, step2, n, riemann_zeta_4, I_m;

  step1 = 1.0;
  step2 = 0.0;
  n = 1.0;
  a_n = 0.0;
  while ( fabs(step1-step2)>1E-6 )
  {
    step1 = step2;
    step2 = 1.0/( exp(n/theta)*n*n ) * ( 0.5+theta/n+theta*theta/n/n );

    a_n += step2;
    n = n + 1.0;
  }

  riemann_zeta_4 = M_PI*M_PI*M_PI*M_PI/90.0;
  I_m = theta * log( 1.0-exp(-1.0/theta) ) + 6.0*theta*theta * (riemann_zeta_4*theta*theta - a_n);

  phi_3 = 0.25 + 2.0*I_m;

  return phi_3;
}




/**
 * \fn int nxs_initUnitCell( NXS_UnitCell *uc )
 * Using SgInfo library function this initialises the unit cell (including its volume) and resets it if atoms have been added before.
 * @param uc NXS_UnitCell struct
 * @return
 */
int nxs_initUnitCell( NXS_UnitCell *uc )
{
  /* at first some initialization for SgInfo */
  T_SgInfo SgInfo;
  const T_TabSgName *tsgn = NULL;
  double a,b,c,alpha,beta,gamma;

  SgInfo.MaxList = 192;
  SgInfo.ListSeitzMx = malloc( SgInfo.MaxList * sizeof(*SgInfo.ListSeitzMx) );
  /* no list info needed here */
  SgInfo.ListRotMxInfo = NULL;

  if( isdigit(uc->spaceGroup[0]) )
  {
    tsgn = FindTabSgNameEntry(uc->spaceGroup, 'A');
    if (tsgn == NULL) return -1; /* no matching table entry */
    strncpy(uc->spaceGroup,tsgn->HallSymbol,MAX_CHARS_SPACEGROUP);
  }

  /* initialize SgInfo struct */
  InitSgInfo( &SgInfo );
  SgInfo.TabSgName = tsgn;
  if ( tsgn )
    SgInfo.GenOption = 1;

  ParseHallSymbol( uc->spaceGroup, &SgInfo );
  CompleteSgInfo( &SgInfo );
  Set_si( &SgInfo );

  uc->crystalSystem = SgInfo.XtalSystem;
  uc->sgInfo = SgInfo;

  /* get the unit cell volume depending on crystal system*/
  uc->volume = 0.0;
  a = uc->a;
  b = uc->b;
  c = uc->c;
  alpha = uc->alpha;
  beta = uc->beta;
  gamma = uc->gamma;

  /* V = a * b * c * sqrt(1 - cos(alpha)^2 - cos(beta)^2 - cos(gamma)^2
                            + 2 * cos(alpha) * cos(beta) * cos(gamma))
  */
  switch( uc->crystalSystem )
  {
    /* XS_Cubic */
    case 7:   uc->volume = a*a*a;
              break;
    /* XS_Hexagonal */
    case 6:   uc->volume = 0.866025*a*a*c; /* = a*a*c * sin(M_PI/3.0) */
              break;
    /* XS_Trigonal */
    case 5:   uc->volume = 0.866025*a*a*c; /* = a*a*c * sin(M_PI/3.0) */
              break;
    /* XS_Tetragonal */
    case 4:   uc->volume = a*a*c;
              break;
    /* XS_Orthorhombic */
    case 3:   uc->volume = a*b*c;
              break;
    /* XS_Monoclinic */
    case 2:   uc->volume = a*b*c * sin(beta);
              break;
    /* XS_Triclinic */
    case 1:   uc->volume = a*b*c * sqrt( 1.0 - cos(alpha)*cos(alpha) - cos(beta)*cos(beta) - cos(gamma)*cos(gamma) + 2.0*cos(alpha)*cos(beta)*cos(gamma) );
              break;
    /* XS_Unknown */
    case 0:   break;
    default:  break;
  }

#ifdef DEBUG
  printf( "\n# --------------------\n# ");
  PrintFullHM_SgName(tsgn, ' ', stdout);
  printf( "\n# Crystal system: %s\n", XS_Name[uc->crystalSystem] );
  printf( "# TSGName: %s %i %s %s\n", SgInfo.TabSgName->HallSymbol, SgInfo.TabSgName->SgNumber,
          SgInfo.TabSgName->Extension, SgInfo.TabSgName->SgLabels );
  printf( "# Centric: %i\n", SgInfo.Centric );
  printf( "# LoopInv: %i\n", Sg_nLoopInv(&SgInfo) );
  printf( "# nList: %i\n", SgInfo.nList );
  printf( "# OriginShift: %i %i %i\n", SgInfo.OriginShift[0], SgInfo.OriginShift[1], SgInfo.OriginShift[2]);
  printf( "# OrderL: %i\n", SgInfo.OrderL );
  printf( "# OrderP: %i\n", SgInfo.OrderP );
  printf( "# Point group: %s\n", PG_Names[PG_Index(SgInfo.PointGroup)] );
  printf( "# nGenerator: %i\n", SgInfo.nGenerator );
  printf( "# GeneratorList: %i %i %i %i\n", SgInfo.Generator_iList[0], SgInfo.Generator_iList[1],
          SgInfo.Generator_iList[2], SgInfo.Generator_iList[3] );
#endif

  uc->mass = 0.0;
  uc->density = 0.0;
  uc->nAtomInfo = 0;
  uc->atomInfoList = NULL;

  return 0;
}



/**
 * \fn int nxs_addAtomInfo( NXS_UnitCell *uc, NXS_AtomInfo ai )
 * This function add an \see NXS_AtomInfo struct to the unit cell. The Wyckoff atom positions are calculated implicitly.
 * @param uc NXS_UnitCell struct
 * @param ai NXS_AtomInfo struct
 * @return
 */
int nxs_addAtomInfo( NXS_UnitCell *uc, NXS_AtomInfo ai )
{
  double temperature = 293.0;
  uc->nAtomInfo++;
  uc->atomInfoList = (NXS_AtomInfo*)realloc( uc->atomInfoList, sizeof(NXS_AtomInfo)*uc->nAtomInfo );

  ai.phi_1 = calcPhi_1( temperature/ai.debyeTemp );
  ai.B_iso = 5.7451121E3 * ai.phi_1 / ai.molarMass / ai.debyeTemp;
  ai.phi_3 = calcPhi_3( temperature/ai.debyeTemp );

  uc->atomInfoList[uc->nAtomInfo-1] = ai;
  generateWyckoffPositions( uc );

  uc->mass += (double)(uc->atomInfoList[uc->nAtomInfo-1].nAtoms) * uc->atomInfoList[uc->nAtomInfo-1].molarMass;

  return 0;
}






/**
 * \fn int nxs_initHKL( NXS_UnitCell *uc )
 * Using SgInfo library function this initialises the hkl reflections.
 * @param uc UnitCell struct
 * @return
 */
int nxs_initHKL( NXS_UnitCell *uc )
{
  unsigned int ai, i,j;
  double tmp;
  T_SgInfo SgInfo;
  int minH, minK, minL, max_hkl, restriction, h,k,l;
  NXS_HKL *hkl;
  unsigned long index_count;

  uc->density = uc->mass / uc->volume / AVOGADRO * 1E24; // [g/cm^3]

#ifdef DEBUG
  {
  int pos;
  printf( "# Generated Positions:\n" );
  for( ai=0; ai<uc->nAtomInfo; ai++ )
    for( pos=0; pos<uc->atomInfoList[ai].nAtoms; pos++ )
      printf( "#   %s   %.3f, %.3f, %.3f\n", uc->atomInfoList[ai].label,uc->atomInfoList[ai].x[pos],
              uc->atomInfoList[ai].y[pos], uc->atomInfoList[ai].z[pos] );
  printf( "Rel. Unit cell mass: %f [g/mol]\n", uc->mass );
  printf( "Unit cell volume: %f [AA^3]\n", uc->volume );
  printf( "Unit cell density: %f [g/cm^3]\n", uc->density );
  }
#endif

  /* at first calculate average sigmaCoherent and sigmaIncoherent for unit cell */

  tmp = 0.0;
  uc->avgSigmaIncoherent = 0.0;
  uc->avgSigmaCoherent = 0.0;

  for( ai=0; ai<uc->nAtomInfo; ai++ )
  {
    uc->avgSigmaCoherent += uc->atomInfoList[ai].b_coherent * uc->atomInfoList[ai].nAtoms;
    uc->avgSigmaIncoherent += uc->atomInfoList[ai].sigmaIncoherent * uc->atomInfoList[ai].nAtoms;
    tmp += uc->atomInfoList[ai].b_coherent * uc->atomInfoList[ai].b_coherent * uc->atomInfoList[ai].nAtoms;
  }
  tmp = tmp / uc->nAtoms;
  uc->avgSigmaIncoherent = uc->avgSigmaIncoherent / uc->nAtoms;
  uc->avgSigmaCoherent = uc->avgSigmaCoherent / uc->nAtoms;
  uc->avgSigmaCoherent = uc->avgSigmaCoherent * uc->avgSigmaCoherent;

  uc->avgSigmaIncoherent += 0.04*M_PI * ( tmp - uc->avgSigmaCoherent );
  uc->avgSigmaCoherent *= 0.04*M_PI;

  /* some initialization for SgInfo */
  SgInfo = uc->sgInfo;

  /* start calculation of permitted reflections and multiplicities */
  max_hkl = uc->maxHKL_index;

  SetListMin_hkl( &SgInfo, max_hkl, max_hkl, &minH, &minK, &minL );

  /* how much hkl indices */
  index_count = (max_hkl-minH+1)*(max_hkl-minH+1)*(max_hkl-minH+1);
  hkl = (NXS_HKL*)malloc(  sizeof(NXS_HKL)*index_count );

  i = 0;

  /* initialize all hkl indices */
  //   for (h=minH; h<=max_hkl; h++)
  //   for (k=minK; k<=max_hkl; k++)
  //   for (l=minL; l<=max_hkl; l++)
  for( h=max_hkl; h>=minH; h-- )
  for( k=max_hkl; k>=minK; k-- )
  for( l=max_hkl; l>=minL; l-- )
  {
    /* do not show hkls that are systematic absent for the space group */
    if( !IsSysAbsent_hkl( &SgInfo, h, k, l, &restriction ) )
    {
      char is_exclusive;
      /* exclude (hkl)=(000) */
      if( h==0 && k==0 && l==0 )
        continue;

      hkl[i].h = h;
      hkl[i].k = k;
      hkl[i].l = l;

      /* check if equivalent plane has been found before (and calculated) */
      is_exclusive = 1;
      for( j=0; j<i; j++ )
      {
        if( AreSymEquivalent_hkl(&SgInfo, h, k, l, hkl[j].h, hkl[j].k, hkl[j].l) )
        {
          is_exclusive = 0;
          /* sort hkl indices */
          if( 1E4*h+1E2*k+l > 1E4*hkl[j].h+1E2*hkl[j].k+hkl[j].l )
          {
            hkl[j].h = h;
            hkl[j].k = k;
            hkl[j].l = l;
          }
        }
        if( !is_exclusive )
          continue;
      }

      if( is_exclusive )
        i++;
    }
  }
  /* reduce the allocated memory */
  uc->nHKL = i;
  hkl = (NXS_HKL*)realloc( hkl, sizeof(NXS_HKL)*uc->nHKL );

  for( i=0; i<uc->nHKL; i++ )
  {
    /* store the equivalent lattice plane (hkl) */
    T_Eq_hkl eqHKL;
    NXS_EquivHKL *equivHKL;
    unsigned int nEqHKL;

    hkl[i].multiplicity = BuildEq_hkl( &SgInfo, &eqHKL, hkl[i].h, hkl[i].k, hkl[i].l );
    equivHKL = (NXS_EquivHKL*)malloc( sizeof(NXS_EquivHKL)*eqHKL.N );

    nEqHKL = eqHKL.N;
    for( j=0; j<nEqHKL; j++ )
    {
      equivHKL[j].h = eqHKL.h[j];
      equivHKL[j].k = eqHKL.k[j];
      equivHKL[j].l = eqHKL.l[j];
    }
    hkl[i].equivHKL = equivHKL;

    /* get d-spacing and |F|^2 */
    hkl[i].dhkl = calcDhkl( hkl[i].h, hkl[i].k, hkl[i].l, uc );
    hkl[i].FSquare = calcFSquare( &(hkl[i]), uc );
  }
  /* end of initalizing */

  /* sort hkl lattice planes by d_hkl */
  qsort( hkl, uc->nHKL, sizeof(NXS_HKL), dhkl_compare );

  uc->hklList = hkl;
  return 0;
}





/**
 * \fn double nxs_CoherentElastic( double lambda, NXS_UnitCell* uc )
 * This function calculates the coherent elastic scattering cross section.
 * \f$\sigma^{coh}_{el}\ [barn = 10^{-24} cm^2]\f$
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return
 */
double nxs_CoherentElastic( double lambda, NXS_UnitCell* uc )
{
  NXS_HKL *hkl = uc->hklList;

  double xsect_coh_el = 0.0;
  // double energy = 8.18042531017E-2/(lambda*lambda);

  /* for all d-spacings... */
  unsigned int i;
  for( i=0; i<uc->nHKL; ++i )
  {
    double delta = lambda - 2.0*hkl[i].dhkl;
    if( delta<1E-6 )
    {
      /* calculate the elastic coherent cross section */
      xsect_coh_el += hkl[i].FSquare * hkl[i].multiplicity * hkl[i].dhkl;
    }
  }
  /* this is our final coherent elastic scattering cross section */
  xsect_coh_el = xsect_coh_el*1E-2 * lambda*lambda / (2.0*uc->volume); // / unitcell.nAtoms;

  return xsect_coh_el;
}




/**
 * \fn double nxs_Absorption( double lambda, NXS_UnitCell* uc )
 * This function calculates the absorbtion cross section.
 * \f$\sigma_{abs}\ [barn = 10^{-24} cm^2]\f$
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return
 */
double nxs_Absorption( double lambda, NXS_UnitCell* uc )
{
  double xsect_abs, sigma = 0.0;
  unsigned int i;
  for( i=0; i<uc->nAtomInfo; i++ )
  {
    sigma += uc->atomInfoList[i].sigmaAbsorption * uc->atomInfoList[i].nAtoms;
  }
  xsect_abs = sigma / 1.798 * lambda;

  return xsect_abs;
}




/**
 * \fn double nxs_IncoherentElastic( double lambda, NXS_UnitCell* uc )
 * This function calculates the coherent elastic scattering cross section.
 * \f$\sigma^{inc}_{el}\ [barn = 10^{-24} cm^2]\f$
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return
 */
double nxs_IncoherentElastic( double lambda, NXS_UnitCell* uc )
{
  double xsect_inc_el, s_el_inc = 0.0;
  unsigned int i;
  for( i=0; i<uc->nAtomInfo; i++ )
  {
    double value = lambda*lambda / 2.0 / uc->atomInfoList[i].B_iso;
    s_el_inc += value * ( 1.0 - exp(-1.0/value) ) * uc->atomInfoList[i].nAtoms;
  }

  xsect_inc_el = s_el_inc * uc->avgSigmaIncoherent;
  return xsect_inc_el;
}




/**
 * \fn double nxs_IncoherentInelastic( double lambda, NXS_UnitCell* uc )
 * This function calculates the coherent elastic scattering cross section.
 * \f$\sigma^{inc}_{inel}\ [barn = 10^{-24} cm^2]\f$
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return
 */
double nxs_IncoherentInelastic( double lambda, NXS_UnitCell* uc )
{
  unsigned int i;
  double phi1_phi3, value, s_el_inc, A,  s_total_inc;
  double xsect_inc_inel = 0.0;

  for( i=0; i<uc->nAtomInfo; i++ )
  {
    phi1_phi3 = uc->atomInfoList[i].phi_1 * uc->atomInfoList[i].phi_3;
    value = lambda*lambda / 2.0 / uc->atomInfoList[i].B_iso;
    s_el_inc = value * ( 1.0 - exp(-1.0/value) );
    A = uc->atomInfoList[i].molarMass*ATOMIC_MASS_U_kg / MASS_NEUTRON_kg;
    s_total_inc = A/(A+1.0)*A/(A+1.0) * ( 1.0 + 9.0 * phi1_phi3 * value/A/A );
    xsect_inc_inel += (s_total_inc - s_el_inc) * uc->atomInfoList[i].nAtoms;
  }

  xsect_inc_inel = xsect_inc_inel * uc->avgSigmaIncoherent;
  return xsect_inc_inel;
}




/**
 * \fn double nxs_CoherentInelastic( double lambda, NXS_UnitCell* uc )
 * This function calculates the coherent inelastic scattering cross section.
 * \f$\sigma^{coh}_{inel}\ [barn = 10^{-24} cm^2]\f$
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return
 */
double nxs_CoherentInelastic( double lambda, NXS_UnitCell* uc )
{
  unsigned int i;
  double phi1_phi3, value, s_el_inc, A,  s_total_inc;
  double xsect_coh_inel = 0.0;

  for( i=0; i<uc->nAtomInfo; i++ )
  {
    phi1_phi3 = uc->atomInfoList[i].phi_1 * uc->atomInfoList[i].phi_3;
    value = lambda*lambda / 2.0 / uc->atomInfoList[i].B_iso;
    s_el_inc = value * ( 1.0 - exp(-1.0/value) );
    A = uc->atomInfoList[i].molarMass*ATOMIC_MASS_U_kg / MASS_NEUTRON_kg;
    s_total_inc = A/(A+1.0)*A/(A+1.0) * ( 1.0 + 9.0 * phi1_phi3 * value/A/A );
    xsect_coh_inel += (s_total_inc - s_el_inc) * uc->atomInfoList[i].nAtoms;
  }

  xsect_coh_inel = xsect_coh_inel * uc->avgSigmaCoherent;
  return xsect_coh_inel;
}




/**
 * \fn double nxs_TotalInelastic( double lambda, NXS_UnitCell* uc )
 * This function calculates the total inelastic scattering cross section.
 * It is faster than calculating \see nxsCoherentInelastic and \see nxsIncoherentInelastic each.
 * \f$\sigma^{total}_{inel}\ [barn = 10^{-24} cm^2]\f$
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return
 */
double nxs_TotalInelastic( double lambda, NXS_UnitCell* uc )
{
  unsigned int i;
  double phi1_phi3, value, s_el_inc, A,  s_total_inc;
  double xsect_total_inel = 0.0;

  for( i=0; i<uc->nAtomInfo; i++ )
  {
    phi1_phi3 = uc->atomInfoList[i].phi_1 * uc->atomInfoList[i].phi_3;
    value = lambda*lambda / 2.0 / uc->atomInfoList[i].B_iso;
    s_el_inc = value * ( 1.0 - exp(-1.0/value) );
    A = uc->atomInfoList[i].molarMass*ATOMIC_MASS_U_kg / MASS_NEUTRON_kg;
    s_total_inc = A/(A+1.0)*A/(A+1.0) * ( 1.0 + 9.0 * phi1_phi3 * value/A/A );
    xsect_total_inel += (s_total_inc - s_el_inc) * uc->atomInfoList[i].nAtoms;
  }

  return xsect_total_inel * (uc->avgSigmaCoherent+uc->avgSigmaIncoherent);
}





NXS_MarchDollase nxs_initMarchDollase( NXS_UnitCell* uc )
{
  NXS_MarchDollase md;
  double n;
  unsigned int i;

  md.N = 101;
  md.M = 1001;
  md.nOrientations = 0;
  md.texture = NULL;
  md.unitcell = uc;

  /* calculate sin_phi and cos_phi */
  md.sin_phi = (double*)malloc( sizeof(double)*md.N );
  md.cos_phi = (double*)malloc( sizeof(double)*md.N );

  n = (double)md.N;
  for( i=0; i<n; i++ )
  {
    md.sin_phi[i] = sin( M_PI/n*(double)i - M_PI/2.0 );
    md.cos_phi[i] = cos( M_PI/n*(double)i - M_PI/2.0 );
  }
  return md;
}




/* add texture and do some pre-calulation to avoid multiple evaluation */
void nxs_addTexture( NXS_MarchDollase* md, NXS_Texture texture )
{
  int a = texture.a;
  int b = texture.b;
  int c = texture.c;
  int h,k,l;
  unsigned int i,j;
  double cos_beta, sin_beta,  N_Nplus1, r, m;

  NXS_UnitCell *uc;
  NXS_HKL *hkl;

  if( a==0 && b==0 && c==0 )
    return;

  //   a = 1.0*texture.a + 0.0*texture.b + 0.0*texture.c;
  //   b = 0.0*texture.a + 0.0*texture.b - 1.0*texture.c;
  //   c = 0.0*texture.a + 1.0*texture.b + 0.0*texture.c;

  uc = md->unitcell;
  hkl = uc->hklList;

  /* calculate sin_beta and cos_beta for all planes */
  texture.sin_beta = (double**)malloc( sizeof(double*)*uc->nHKL );
  texture.cos_beta = (double**)malloc( sizeof(double*)*uc->nHKL );

  for( i=0; i<uc->nHKL; i++ )
  {
    unsigned int nEquivalent = hkl[i].multiplicity / 2;
    NXS_EquivHKL *equivHKL;

    if( nEquivalent==0 ) nEquivalent = 1;
    equivHKL = hkl[i].equivHKL;

    texture.sin_beta[i] = (double*)malloc( sizeof(double)*nEquivalent );
    texture.cos_beta[i] = (double*)malloc( sizeof(double)*nEquivalent );

    for( j=0; j<nEquivalent; j++ )
    {
      h = equivHKL[j].h;
      k = equivHKL[j].k;
      l = equivHKL[j].l;

      cos_beta = (double)(a*h+b*k+c*l) / sqrt((double)(a*a+b*b+c*c)) / sqrt((double)(h*h+k*k+l*l));
      if ( cos_beta > 1.0 )
        cos_beta = 1.0;
      else if ( cos_beta < -1.0 )
        cos_beta = -1.0;
      sin_beta = sin( acos(cos_beta) );

      texture.sin_beta[i][j] = sin_beta;
      texture.cos_beta[i][j] = cos_beta;
    }
  }

  /* calculate P_alpha_H in constant cos_alpha_H steps */
  N_Nplus1 = 1.0 /(double)(md->N+1.0);
  r = texture.r;
  texture.P_alpha_H = (double*)malloc( sizeof(double)*md->M );

  m = (double)md->M;
  for ( i=0; i<md->M; i++ )
  {
    double  cos_alpha_H, sin_alpha_H;
    cos_alpha_H = 2.0/m*i - 1.0;
    sin_alpha_H = sin( acos(cos_alpha_H) );
    texture.P_alpha_H[i] = pow( r*r*cos_alpha_H*cos_alpha_H + sin_alpha_H*sin_alpha_H/r, -1.5 ) * N_Nplus1;
  }

  md->nOrientations++;
  md->texture = (NXS_Texture*)realloc( md->texture, sizeof(NXS_Texture)*md->nOrientations );
  md->texture[md->nOrientations-1] = texture;
}




double nxs_CoherentElasticTexture( double lambda, NXS_MarchDollase* md )
{
  double xsect_coh_el = 0.0;
  // double energy = 8.18042531017E-2/(lambda*lambda);

  //  double x = 1.0;
  //  double y = 0.0;
  //  double z = 0.0;
  //  double length_xyz = sqrt( x*x + y*y + z*z );
  //   x = x/length_xyz;
  //   y = y/length_xyz;
  //   z = z/length_xyz;

  NXS_UnitCell *uc = md->unitcell;
  NXS_HKL *hkl = uc->hklList;
  NXS_Texture *texture = md->texture;
  unsigned int nEquivalent;

  /* for all d-spacings... */
  unsigned int i, j, k, l;

  for( i=0; i<uc->nHKL; i++ )
  {
    double delta = lambda - 2.0*hkl[i].dhkl;
    if( delta < -1E-6 )
    {
      double alpha_h, sin_alpha_h, cos_alpha_h,  cos_beta, sin_beta;
      double corr = 0.0;
      nEquivalent = hkl[i].multiplicity / 2;
      if( nEquivalent==0 ) nEquivalent = 1;

      /* calculate alpha_h */
      alpha_h = M_PI/2.0 - asin( lambda/2.0/hkl[i].dhkl );
      sin_alpha_h = sin( alpha_h );
      cos_alpha_h = cos( alpha_h );

      /* calculate the correction factor... */
      for( j=0; j<md->nOrientations; j++ )
      {
        for( k=0; k<nEquivalent; k++ )
        {
          cos_beta = texture[j].cos_beta[i][k];
          sin_beta = texture[j].sin_beta[i][k];
          for( l=0; l<md->N; l++ )
          {
            double sin_phi, a, cos_alpha_H;
            unsigned int index;
            sin_phi = md->sin_phi[l];
            // double cos_phi = md->cos_phi[l];

            a = cos_beta*cos_alpha_h - sin_beta*sin_alpha_h*sin_phi;
            //             double b = sin_beta*cos_phi;
            //             double c = cos_beta*sin_alpha_h + sin_beta*cos_alpha_h*sin_phi;
            cos_alpha_H = a/* + b + c*/;
            //             cos_alpha_H = a*x+b*y+c*z;

            index = (int)( (1.0+cos_alpha_H)/2.0*(double)md->M );
            corr += md->texture[j].P_alpha_H[index] * md->texture[j].f;
          }
        } /* end of nEquivalent */
      } /* end of nOrientations */

      /* calculate the elastic coherent cross section with texture influence */
      corr = corr / (double)(nEquivalent);
      xsect_coh_el += hkl[i].FSquare * hkl[i].multiplicity * hkl[i].dhkl * corr;
    }
  } /* end of hkls */

  /* this is our final coherent elastic scattering cross section with texture influence*/
  xsect_coh_el = xsect_coh_el*1E-2 * lambda*lambda / (2.0*uc->volume); // uc->nAtoms;

  return xsect_coh_el;
}




const char *NXS_keys[] =
{
  "space_group",
  "lattice_a",
  "lattice_b",
  "lattice_c",
  "lattice_alpha",
  "lattice_beta",
  "lattice_gamma",
  "add_atom"
};




int nxs_readParameterFile( const char* fileName, NXS_UnitCell *uc )
{
  unsigned int numAtoms = 0;
  NXS_AtomInfo ai;
  NXS_AtomInfo *aiArray = NULL;

  char line[200];
  unsigned int max_keys = array_length(NXS_keys);

  /* open the parameter file */
  FILE* file;
  file = fopen(fileName, "r");

  if (!file)
  {
    return -1;
  }



  while( fgets(line, sizeof(line), file) != NULL )
  {
    /* make sure 1st character isn't a space */
    char *ptr = line;
  char *par;
    while( *ptr && isspace(*ptr) )
      ptr++;

    /* find parameter and value pair */
    par = strtok( ptr, "=" );
    if( par != NULL )
    {
      unsigned int i=0;
      /* remove possible spaces from the end of the parameter term */
      char *key = par;
      while( isspace(key[strlen(key)-1]) )
        key[strlen(key)-1] = '\0';

      while( i < max_keys && strcmp(key, NXS_keys[i]) )
        i++;
      /* parameter found, now check the value */
      if( i < max_keys )
      {
      char *endptr;
        par = strtok( NULL, "=#!;" );

        /* remove possible spaces from begin and end of the value term */
        while( *par && isspace(*par) )
          par++;
        while( isspace(par[strlen(par)-1]) )
          par[strlen(par)-1] = '\0';

        switch( i )
        {
          case 0:                                    /* space_group */
            strncpy(uc->spaceGroup,par,MAX_CHARS_SPACEGROUP); break;
          case 1:                                    /* lattice_a */
            uc->a = strtod(par, &endptr); break;
          case 2:                                    /* lattice_b */
            uc->b = strtod(par, &endptr); break;
          case 3:                                    /* lattice_c */
            uc->c = strtod(par, &endptr); break;
          case 4:                                    /* lattice_alpha */
            uc->alpha = strtod(par, &endptr); break;
          case 5:                                    /* lattice_beta */
            uc->beta = strtod(par, &endptr); break;
          case 6:                                    /* lattice_gamma */
            uc->gamma = strtod(par, &endptr); break;
          case 7:                                    /* add_atom */
            strncpy(ai.label,strtok(par, " "),MAX_CHARS_ATOMLABEL);     /* atom name or label */
            ai.b_coherent = strtod(strtok( NULL, " " ), &endptr);       /* b_coh */
            ai.sigmaIncoherent = strtod(strtok( NULL, " " ), &endptr);  /* sigma_inc */
            ai.sigmaAbsorption = strtod(strtok( NULL, " " ), &endptr);  /* sigma_abs_2200 */
            ai.molarMass = strtod(strtok( NULL, " " ), &endptr);        /* molar_mass */
            ai.debyeTemp = strtod(strtok( NULL, " " ), &endptr);        /* debye_temp */
            ai.x[0] = strtod(strtok( NULL, " " ), &endptr);             /* x */
            ai.y[0] = strtod(strtok( NULL, " " ), &endptr);             /* y */
            ai.z[0] = strtod(strtok( NULL, " " ), &endptr);             /* z */
            /* collect all "add_atom" entries from file to make sure     */
            /* uc is initialised first before nxs_addAtomInfo is applied */
            aiArray = (NXS_AtomInfo*)realloc( aiArray, sizeof(NXS_AtomInfo)*(++numAtoms) );
            aiArray[numAtoms-1] = ai;
            break;
          default:  break;

        } /* end of: switch( i ) */
      } /* end of: if( i < max_keys ) */
    } /* end of: if( par != NULL ) */
  } /* end of: while( fgets(line, sizeof(line), file) != NULL ) */

  /* finished with reading file */
  fclose(file);
  if( numAtoms )
  {
    unsigned int i=0;

    /* initialise unit cell and add atoms */
    nxs_initUnitCell( uc );
    for( i=0; i<numAtoms; i++ )
      nxs_addAtomInfo( uc, aiArray[i] );
  }
  else
  {
    /* TODO: ERROR HANDLING */
    return -1;
  }
  return 0;
}




int nxs_saveParameterFile( const char* fileName, NXS_UnitCell *uc)
{
  unsigned int i = 0;

  /* open the parameter file */
  FILE* file;
  file = fopen(fileName, "w");

  if (!file)
  {
    /* TODO: ERROR HANDLING */
    return -1;
  }

  fprintf( file, "#\n# This is an nxs parameter file\n#\n%s = %s\n%s = %f\n%s = %f\n%s = %f\n%s = %f\n%s = %f\n%s = %f\n\n# label  b_coherent  sigma_inc  sigma_abs  molar_mass  debye_temp  x  y  z\n",
        NXS_keys[0], uc->spaceGroup, NXS_keys[1], uc->a, NXS_keys[2], uc->b, NXS_keys[3], uc->c, NXS_keys[4], uc->alpha, NXS_keys[5], uc->beta, NXS_keys[6], uc->gamma);

  for(i=0; i<uc->nAtomInfo; i++)
  {
    fprintf( file, "%s = %s %f %f %f %f %f %f %f %f\n",
         NXS_keys[7], uc->atomInfoList[i].label, uc->atomInfoList[i].b_coherent, uc->atomInfoList[i].sigmaIncoherent,
         uc->atomInfoList[i].sigmaAbsorption, uc->atomInfoList[i].molarMass, uc->atomInfoList[i].debyeTemp,
         uc->atomInfoList[i].x[0], uc->atomInfoList[i].y[0], uc->atomInfoList[i].z[0] );
  }

  fclose(file);

  return 0;
}
