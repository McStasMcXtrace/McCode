/*!
    \section abstract_sec Abstract
    nxs: a program library for neutron cross section calculations
    Purpose: Calculates neutron scattering and absorption cross sections for
    polycrystalline materials based on the composition of a crystallographic unit
    cell.

    @author Mirko Boin, Helmholtz-Zentrum Berlin f&uuml;r Materialien und Energy GmbH, <boin@helmholtz-berlin.de>
    @version 1.4

*/

/*! \mainpage nxs - a library for neutron cross section calculations

    \section intro_sec Introduction
    The nxs library for computing neutron scattering and absorption cross sections provides
    a number of C structs and functions to calculate wavelength-dependent cross section values for
    polycrystalline/powder-like materials. The definition of a material is represented by the composition
    of a unit cell (NXS_UnitCell). A unit cell is created from the specification of a space group and its
    unit cell parameters. The SgInfo routines from Ralf W. Grosse-Kunstleve is included here for such
    purposes. Monoatomic materials as well as multi-atomic compounds are created by adding NXS_AtomInfo
    atom information/properties. The library also provides a reading and saving routines to compose unit
    cells from nxs parameter files.


    \section usage_sec Usage

    Example:

    The below example shows howto quickly use the library routines to initialise a unit cell from a parameter
    file and calculate some cross sections.
    \code{.c}
      NXS_UnitCell uc;
      if( NXS_ERROR_OK == nxs_initFromParameterFile( "Al.nxs", &uc ) )
      {
        double lambda=0.1;
        for( lambda=0.1; lambda<4.0; lambda+=0.1 )
          printf("%f\n",nxs_Absorption(lambda, &uc ) );
      }
    \endcode

    A more comprehensive example shows use of individual atom parameters for the construction of a unit
    cell and the initialization of hkl lattice reflections in order to prepare for cross section calculations.
    \code{.c}
      NXS_UnitCell uc;
      NXS_AtomInfo *atomInfoList;
      int numAtoms = nxs_readParameterFile( "Al.nxs", &uc, &atomInfoList);
      if( numAtoms > 0 )
      {
        int i=0;
        nxs_initUnitCell(&uc);
        for( i=0; i<numAtoms; i++ )
          nxs_addAtomInfo( &uc, atomInfoList[i] );

        nxs_initHKL( &uc );

        double lambda=0.1;
        for( lambda=0.1; lambda<4.0; lambda+=0.1 )
        {
          printf("%f\n",nxs_Absorption(lambda, &uc ) );
        }
      }
    \endcode

    \section parameterfiles_sec Parameter files
    Currently, nxs provides routines to read and save nxs parameter files of one particular kind. It is a
    human-readable, INI-like, format to store the necessary information for the compositon of a crystallographic
    unit cell. An example of NaCl is given below:

    \verbatim
    #
    # This is an nxs parameter file
    #

    # define the unit cell parameters:
    #   space_group                      - the space group number or Hermann or Hall symbol [string]
    #   lattice_a, ...b, ...c            - the lattice spacings a,b,c [angstrom]
    #   lattice_alpha, ...beta, ...gamma - the lattice angles alpha,beta,gamma [degree]
    #   debye_temp                       - the Debye temperature [K]

    space_group=225
    lattice_a=5.64
    lattice_c=4.95
    lattice_alpha=90
    debye_temp=320

    # add atoms to the unit cell:
    # notation is "atom_number = name b_coh sigma_inc sigma_abs_2200 molar_mass x y z"
    #   name           - labels the current atom/isotope  [string]
    #   b_coh          - the coherent scattering length [fm]
    #   sigma_inc      - the incoherent scattering cross section [barns]
    #   sigma_abs_2200 - the absorption cross sect. at 2200 m/s [barns]
    #   molar_mass     - the Molar mass [g/mol]
    #   x y z          - the Wyckoff postion of the atom inside the unit cell
    #
    # e.g.: add_atom = Fe 9.45 0.4 2.56 55.85 0.0 0.0 0.0

    [atoms]
    add_atom=Na 3.63 1.62 0.53 22.99 0.0 0.0 0.0
    add_atom=Cl 9.577 5.3 33.5 35.45 0.5 0.5 0.5
    \endverbatim

    \section copyright_sec Copyright
    nxs - neutron cross sections (c) 2010-2014 Mirko Boin

    The nxs library includes the SgInfo library, whose free usage is granted by the following notice:

    Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve
    Permission to use and distribute this software and its documentation for noncommercial
    use and without fee is hereby granted, provided that the above copyright notice appears
    in all copies and that both that copyright notice and this permission notice appear in
    the supporting documentation. It is not allowed to sell this software in any way. This
    software is not in the public domain.


    \section references_sec References

    \subsection mainreference_sec Main references

      \li Boin, M. (2012). J. Appl. Cryst. 45. 603-607. doi: 10.1107/S0021889812016056

    \subsection appreference_sec Applications of nxs

      \li Boin, M.; Hilger, A.; Kardjilov, N.; Zhang, S. Y.; Oliver, E. C.; James, J. A.; Randau, C. & Wimpory, R. C. (2011). J. Appl. Cryst. 44. 1040-1046. doi: 10.1107/S0021889811025970
      \li Strobl, M.; Hilger, A.; Boin, M.; Kardjilov, N.; Wimpory, R.; Clemens, D.; M&uuml;hlbauer, M.; Schillinger, B.; Wilpert, T.; Schulz, C.; Rolfs, K.; Davies, C. M.; O'Dowd, N.; Tiernan, P. & Manke, I. (2011). Nucl. Instrum. Methods Phys. Res., Sect. A 651. 149-155. doi: 10.1016/j.nima.2010.12.121
      \li Boin, M.; Wimpory, R. C.; Hilger, A.; Kardjilov, N.; Zhang, S. Y. & Strobl, M. (2012). J. Phys: Conf. Ser. 340. 012022. doi: 10.1088/1742-6596/340/1/012022
      \li Kittelmann, T.; Stefanescu, I.; Kanaki, K.; Boin, M.; Hall-Wilton, R. & Zeitelhack, K. (2014). J. Phys: Conf. Ser. 513. 022017. doi: 10.1088/1742-6596/513/2/022017
      \li Peetermans, S.; Tamaki, M.; Hartmann, S.; Kaestner, A.; Morgano, M. & Lehmann, E. H. (2014). Nucl. Instrum. Methods Phys. Res., Sect. A 757. 28-32. doi: 10.1016/j.nima.2014.04.033
 */


#include "nxs.h"

#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>


#ifndef M_PI
  #define M_PI 3.14159265358979323846
#endif


/* GLOBAL CONSTANTS */
/*
static const double BOLTZMANN_CONSTANT_evK = 8.617343E-5; // in [eV/K]
static const double BOLTZMANN_CONSTANT_JK = 1.3806504E-23; // in [J/K]
static const double PLANCK_CONSTANT_eVs = 4.13566733E-15; // in [eVs]
static const double PLANCK_CONSTANT_Js = 6.62606896E-34; // in [Js]
static const double PLANCK_BAR_eVs = 6.58211899E-16; // in [eVs] (h_ = h/2pi)
static const double PLANCK_BAR_Js = 1.054571628E-34; // in [Js]
static const double EV2J = 1.6021773E-19; // 1eV = 1.6021773E-19J
static const double J2EV = 6.2415063E18;
static const double MASS_NEUTRON_kg = 1.674927351E-27; // [kg]
static const double AVOGADRO = 6.0221417930E23; // [mol-1]
static const double ATOMIC_MASS_U_kg = 1.6605402E-27; // atomic_mass in kg
*/



/**
 * \fn static int _dhkl_compare( const void *par1, const void *par2 )
 * \brief Compares to dhkl values.
 *
 * This function is used within nxs_initHKL() to sort the hkl lattice planes.
 *
 * @param par1
 * @param par2
 * @return int
 */
static int _dhkl_compare( const void *par1, const void *par2 )
{
  NXS_HKL* p1 = (NXS_HKL*)par1;
  NXS_HKL* p2 = (NXS_HKL*)par2;

  /* compare dhkl first */
  if( fabs(p1->dhkl-p2->dhkl) > 1.0e-6 ) return p1->dhkl < p2->dhkl ? 1 : -1;
  /* compare FSquare*multiplicity */
  else if( fabs(p1->FSquare*p1->multiplicity - p2->FSquare*p2->multiplicity) > 1.0e-6 ) return p1->FSquare*p1->multiplicity < p2->FSquare*p2->multiplicity ? 1 : -1;
  /* compare h, then k, then l */
  else if( p1->h!=p2->h ) return p2->h < p1->h ? 1 : -1;
  else if( p1->k!=p2->k ) return p2->k < p1->k ? 1 : -1;
  else return p2->l < p1->l ? 1 : -1;
}



//static int _equivhkl_compare( const void *par1, const void *par2 )
//{
//  int hkl1, hkl2;
//  hkl1 = abs( (int)1E6*((NXS_EquivHKL*)par1)->h ) + abs( (int)1E3*((NXS_EquivHKL*)par1)->k ) + abs( ((NXS_EquivHKL*)par1)->l );
//  hkl2 = abs( (int)1E6*((NXS_EquivHKL*)par2)->h ) + abs( (int)1E3*((NXS_EquivHKL*)par2)->k ) + abs( ((NXS_EquivHKL*)par2)->l );
//  return ( (hkl1<hkl2)  ? 1 : 0 );
//}



/**
 * \fn static double _distance( double x1, double y1, double z1, double x2, double y2, double z2 )
 * \brief Calculates the distance between two atom positions (x1,y1,z1) and (x2,y2,z2).
 *
 * This function is used within _generateWyckoffPositions().
 *
 * @param x1
 * @param y1
 * @param z1
 * @param x2
 * @param y2
 * @param z2
 * @return int
 */
static double _distance( double x1, double y1, double z1, double x2, double y2, double z2 )
{
  double dx = x2 - x1;
  double dy = y2 - y1;
  double dz = z2 - z1;

  return sqrt( dx*dx + dy*dy + dz*dz );
}


/**
 * \fn static int _generateWyckoffPositions( NXS_UnitCell *uc )
 * \brief Generates Wyckoff position of a unit cell.
 *
 * Using SgInfo library functions this function generates Wyckoff atom position depending on the symmetry of the unit cell,
 * given via NXS_UnitCell and on the atom information added to it before, e.g. via nxs_addAtomInfo().
 *
 * @param uc NXS_UnitCell struct
 * @return nxs error code
 */
static int _generateWyckoffPositions( NXS_UnitCell *uc )
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
            if( _distance(x,y,z,ai.x[l],ai.y[l],ai.z[l]) < 1E-6 )
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
  return NXS_ERROR_OK;
}


/**
 * \fn nxs_calcFSquare( NXS_HKL *hklReflex, NXS_UnitCell* uc )
 * \brief Calculates the structure factor squared |F<sub>hkl</sub>|<sup>2</sup>.
 *
 * This function calculates the structure factor squared depending on the hkl Miller indices, given
 * via NXS_HKL, and on the crystal system, given via the NXS_UnitCell.
 *
 * @param hklReflex NXS_HKL struct
 * @param uc NXS_UnitCell struct
 * @return |F<sub>hkl</sub>|<sup>2</sup>
 */
double nxs_calcFSquare( NXS_HKL *hklReflex, NXS_UnitCell *uc )
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
 * \fn double nxs_calcDhkl( int h, int k, int l, NXS_UnitCell *uc )
 * \brief Calculates the lattice spacing.
 *
 * This function calculates the lattice spacing in &Aring; depending on the hkl Miller indices and the crystal system,
 * given via the NXS_UnitCell.
 *
 * @param h Miller index h
 * @param k Miller index k
 * @param l Miller index l
 * @param uc NXS_UnitCell struct
 * @return d<sub>hkl</sub> [&Aring;]
 */
double nxs_calcDhkl( int h, int k, int l, NXS_UnitCell *uc )
{
  double d_spacing = 0.0;
  double a = uc->a;
  double b = uc->b;
  double c = uc->c;
  double alpha = uc->alpha;
  double beta = uc->beta;
  double gamma = uc->gamma;
  double t1, t2, t3, t4, t5, t6;

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

              d_spacing = 1.0/uc->volume/uc->volume * ( t1*h*h + t2*k*k + t3*l*l + 2.0*t4*h*k + 2.0*t5*k*l + 2.0*t6*h*l );
              d_spacing = sqrt( 1.0 / d_spacing );
              break;
    /* XS_Unknown */
    case 0:   break;
    default:  break;
  }

  return d_spacing;
}




/**
 * \fn static double _calcPhi_1( double theta )
 * \brief Calculates &phi;<sub>1</sub>(&theta;) as shown by Vogel (2000).
 *
 * References:
 *  - S. Vogel (2000) Thesis, Kiel University, Germany.
 *
 * @param theta = T/&theta;<sub>D</sub>
 * @return &phi;<sub>1</sub>(&theta;)
 */
static double _calcPhi_1( double theta )
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




/**
 * \fn static double _calcPhi_3( double theta )
 * \brief Calculates &phi;<sub>1</sub>(&theta;) as shown by Vogel (2000).
 *
 * References:
 *  - S. Vogel (2000) Thesis, Kiel University, Germany.
 *
 * @param theta = T/&theta;<sub>D</sub>
 * @return &phi;<sub>3</sub>(&theta;)
 */
static double _calcPhi_3( double theta )
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
 * \brief Initializes a unit cell.
 *
 * Based on a space group (uc->spaceGroup) and on the lattice parameters (uc->a,..., uc->alpha,...), given by the user,
 * this function initializes a unit cell and calculates its volume. If atoms, i.e. NXS_AtomInfo, have been added before,
 * e.g. by nxs_addAtomInfo(), then they are lost as this routine resets the unit cell. The unit cell temperature (sample
 * temperature) is set to 293 K. Any wish to change the temperature must be applied after calling this function.
 * Internally, the routine uses SgInfo library function to parse the space group information and obtain the crystal system.
 *
 * @param uc NXS_UnitCell struct
 * @return nxs error code
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
    if (tsgn == NULL)
      return NXS_ERROR_NOMATCHINGSPACEGROUP; /* no matching table entry */
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

  /*
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
  */

  // some default values to start with
  uc->temperature = 293.0;
  uc->mass = 0.0;
  uc->density = 0.0;
  uc->nAtomInfo = 0;
  uc->atomInfoList = NULL;

  if( uc->mph_c2 > 1E-6 )
    uc->__flag_mph_c2 = 0;

  return NXS_ERROR_OK;
}


/* implementation of R factors by Th. Kittelmann after A.K. Freund (1983) Nucl. Instr. Meth. 213, 495-501 */
//static double rfacts[23] = {0};
static double rfacts[23] = { 2./5, -1./7, 1./54, 0., -1./4680, 0., 1./257040, 0.,
                              -1./12700800., 0., 1./598752000, 0., -691./18961278336E3, 0.,
                              1./12329501184E2, 0., -3617./19740652259328E4, 0., 43867./104736431452004352E3, 0.,
                              -174611./18064297410711552E6, 0., 854513./3800246460616091682E6 };

//static void initRfacts()
//{
//  // Bernoulli numbers B_n (only for n = 0...23)
//  double bernoulli[23] = { 1, -1./2, 1./6, 0., -1./30., 0., 1./42, 0.,
//                           -1./30., 0., 5./66, 0., -691./2730., 0.,
//                           7./6, 0., -3617./510., 0., 43867./798, 0.,
//                           -174611./330., 0., 854513./138 };
//  double nfact = 1.0;
//  unsigned int n;
//  for( n=0; n <= 22; ++n )
//  {
//    if (n)
//      nfact *= n;
//    rfacts[n] = bernoulli[n] / (nfact*(n+5.0/2.0));


//    printf("%i: %f = %f\n", n, rfacts[n]*1E20 - rfacts2[n]*1E20 );
//  }
//}

static double calcR(double x)
{
  double sum = 0.0;
  double xx = 1.0/x;
  unsigned int n;

//  // This is done only once at the beginning
//  if( fabs(rfacts[0])<1E-6 )
//    initRfacts();

  for (n=0;n<22;++n)
  {
    sum += xx * rfacts[n];
    xx *= x;
  }
  sum += 0.5 * xx * rfacts[22];
  return sum;
}


/**
 * \fn int nxs_addAtomInfo( NXS_UnitCell *uc, NXS_AtomInfo ai )
 * \brief Adds an atom to a unit cell.
 *
 * This function adds an NXS_AtomInfo to a NXS_UnitCell. The Wyckoff atom positions, the unit cell mass and phi_1,
 * phi3 and B_iso are calculated implicitly.
 *
 * @param uc NXS_UnitCell struct
 * @param ai NXS_AtomInfo struct
 * @return nxs error code
 */
int nxs_addAtomInfo( NXS_UnitCell *uc, NXS_AtomInfo ai )
{
  double x;

  /* if debyeTemp was not given as a parameter */
  if( uc->debyeTemp<1E-6 )
  {
    // maybe a calculation/estimation of debyeTemp from atom-specific Debye temperatures
    // is possible in future; then such a calculation should be called here
    // Instead a standard value is set if no debyeTemp was given before
    uc->debyeTemp = 300.0;
  }

  uc->atomInfoList = (NXS_AtomInfo*)realloc( uc->atomInfoList, sizeof(NXS_AtomInfo)*(uc->nAtomInfo+1) );
  if( !uc->atomInfoList )
    return NXS_ERROR_MEMORYALLOCATIONFAILED;

  uc->nAtomInfo++;
  //ai.M_m = ai.molarMass*ATOMIC_MASS_U_kg/MASS_NEUTRON_kg;
  ai.M_m = ai.molarMass * 0.99140954426;

  // Single phonon part after A.K. Freund (1983) Nucl. Instr. Meth. 213, 495-501
  ai.sph =  sqrt( uc->debyeTemp ) / ai.M_m;
  x = uc->debyeTemp/uc->temperature;
  if( x <= 6 )
    ai.sph *= calcR(x);
  else
    ai.sph *= 3.29708964927644 * pow(x,-3.5);

  // from S. Vogel (2000) Thesis, Kiel University
  ai.phi_1 = _calcPhi_1( 1/x );
  ai.B_iso = 5.7451121E3 * ai.phi_1 / ai.molarMass / uc->debyeTemp;
  ai.phi_3 = _calcPhi_3( 1/x );

  uc->atomInfoList[uc->nAtomInfo-1] = ai;
  _generateWyckoffPositions( uc );

  uc->mass += (double)(uc->atomInfoList[uc->nAtomInfo-1].nAtoms) * uc->atomInfoList[uc->nAtomInfo-1].molarMass;

  //uc->density = uc->mass / uc->volume / AVOGADRO * 1E24; // [g/cm^3]
  uc->density = uc->mass / uc->volume / 0.60221417930; // [g/cm^3]

  /* if mph_c2 was not given as a parameter */
  if( uc->__flag_mph_c2 )
  {
    // from A.K. Freund (1983) Nucl. Instr. Meth. 213, 495-501
    // this needs further investigation as the fit in Freund's paper is not sufficient for all atoms,
    //uc->mph_c2 = 4.27 * exp( uc->mass*ATOMIC_MASS_U_kg/MASS_NEUTRON_kg/uc->nAtoms / 61.0 );
    uc->mph_c2 = 4.27 * exp( uc->mass*0.01625261547/uc->nAtoms );
  }

  return NXS_ERROR_OK;
}


/**
 * \fn int nxs_initHKL( NXS_UnitCell *uc )
 * \brief Initializes the hkl lattice planes.
 *
 * This function uses SgInfo library functions to initialise the hkl planes and implicitly calculates
 * the unit cell density, the C2 multi-phonon constant (if not given by the user before), average
 * coherent and incoherent cross section value and stores equivalent hkl lattice planes (if any) per hkl.
 *
 * @param uc UnitCell struct
 * @return nxs error code
 */
int nxs_initHKL( NXS_UnitCell *uc )
{
  unsigned int ai, i,j;
  double tmp;
  T_SgInfo SgInfo;
  int minH, minK, minL, max_hkl, restriction, h,k,l;
  NXS_HKL *hkl;
  unsigned long index_count;

  /*
  int pos;
  printf( "# Generated Positions:\n" );
  for( ai=0; ai<uc->nAtomInfo; ai++ )
    for( pos=0; pos<uc->atomInfoList[ai].nAtoms; pos++ )
      printf( "#   %s   %.3f, %.3f, %.3f\n", uc->atomInfoList[ai].label,uc->atomInfoList[ai].x[pos],
              uc->atomInfoList[ai].y[pos], uc->atomInfoList[ai].z[pos] );
  printf( "Rel. Unit cell mass: %f [g/mol]\n", uc->mass );
  printf( "Unit cell volume: %f [AA^3]\n", uc->volume );
  printf( "Unit cell density: %f [g/cm^3]\n", uc->density );
  */

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
  if( !hkl )
    return NXS_ERROR_MEMORYALLOCATIONFAILED;

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
          if( 1E6*h+1E3*k+l > 1E6*hkl[j].h+1E3*hkl[j].k+hkl[j].l )
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
  if( !hkl )
    return NXS_ERROR_MEMORYALLOCATIONFAILED;

  for( i=0; i<uc->nHKL; i++ )
  {
    /* store the equivalent lattice plane (hkl) */
    T_Eq_hkl eqHKL;
    NXS_EquivHKL *equivHKL;
    unsigned int nEqHKL;

    hkl[i].multiplicity = BuildEq_hkl( &SgInfo, &eqHKL, hkl[i].h, hkl[i].k, hkl[i].l );
    equivHKL = (NXS_EquivHKL*)malloc( sizeof(NXS_EquivHKL)*eqHKL.N );
    if( !equivHKL )
      return NXS_ERROR_MEMORYALLOCATIONFAILED;

    nEqHKL = eqHKL.N;
    for( j=0; j<nEqHKL; j++ )
    {
      equivHKL[j].h = eqHKL.h[j];
      equivHKL[j].k = eqHKL.k[j];
      equivHKL[j].l = eqHKL.l[j];
    }
    hkl[i].equivHKL = equivHKL;

    /* get d-spacing and |F|^2 */
    hkl[i].dhkl = nxs_calcDhkl( hkl[i].h, hkl[i].k, hkl[i].l, uc );
    hkl[i].FSquare = nxs_calcFSquare( &(hkl[i]), uc );
  }
  /* end of initalizing */

  /* sort hkl lattice planes by d_hkl */
  qsort( hkl, uc->nHKL, sizeof(NXS_HKL), _dhkl_compare );

  uc->hklList = hkl;
  return NXS_ERROR_OK;
}





/**
 * \fn double nxs_CoherentElastic( double lambda, NXS_UnitCell* uc )
 * \brief Calculates the coherent elastic scattering cross section.
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>coh_el</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
double nxs_CoherentElastic( double lambda, NXS_UnitCell* uc )
{
  NXS_HKL *hkl = uc->hklList;

  double xsect_coh_el = 0.0;

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
  xsect_coh_el = xsect_coh_el*1E-2 * lambda*lambda / (2.0*uc->volume);

  return xsect_coh_el;
}




/**
 * \fn double nxs_Absorption( double lambda, NXS_UnitCell* uc )
 * \brief Calculates the absorbtion cross section.
 *
 * This function calculates the absorbtion cross section on the base of a given reference value at a
 * neutron velocity of \f$v = 2200 m/s\f$, i.e. 1.798 &Aring;
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>abs</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
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
 * \brief Calculates the incoherent elastic scattering cross section.
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>inc_el</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
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
 * \brief Calculates the incoherent inelastic scattering cross section.
 *
 * An approximation of the inelastic part by the incoherent cross section is used here.
 * This approach works sufficiently for thermal neutrons as shown by Binder (1970).
 *
 * References:
 *  - K. Binder (1970) Phys. Stat. Sol. 41, 767-779.
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>inc_inel</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
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
    A = uc->atomInfoList[i].M_m;
    s_total_inc = A/(A+1.0)*A/(A+1.0) * ( 1.0 + 9.0 * phi1_phi3 * value/A/A );
    xsect_inc_inel += (s_total_inc - s_el_inc) * uc->atomInfoList[i].nAtoms;
  }

  xsect_inc_inel = xsect_inc_inel * uc->avgSigmaIncoherent;
  return xsect_inc_inel;
}




/**
 * \fn double nxs_CoherentInelastic( double lambda, NXS_UnitCell* uc )
 * \brief Calculates the coherent inelastic scattering cross section.
 *
 * An approximation of the inelastic part by the incoherent cross section is used here.
 * This approach works sufficiently for thermal neutrons as shown by Binder (1970)
 *
 * References:
 *  - K. Binder (1970) Phys. Stat. Sol. 41, 767-779.
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>coh_inel</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
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
    A = uc->atomInfoList[i].M_m;
    s_total_inc = A/(A+1.0)*A/(A+1.0) * ( 1.0 + 9.0 * phi1_phi3 * value/A/A );
    xsect_coh_inel += (s_total_inc - s_el_inc) * uc->atomInfoList[i].nAtoms;
  }
  xsect_coh_inel = xsect_coh_inel * uc->avgSigmaCoherent;
  return xsect_coh_inel;
}



/**
 * \fn double nxs_TotalInelastic( double lambda, NXS_UnitCell* uc )
 * This function is provided for convenience and to be downward compatible to earlier versions of the nxs library.
 * It currently links to nxs_TotalInelastic_BINDER().
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>tot_inel</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
double nxs_TotalInelastic( double lambda, NXS_UnitCell* uc )
{
  return nxs_TotalInelastic_COMBINED( lambda, uc);
}


/**
 * \fn double nxs_TotalInelastic_BINDER( double lambda, NXS_UnitCell* uc )
 * \brief Calculates the total inelastic scattering cross section.
 *
 * It is faster than calculating nxsCoherentInelastic() and nxsIncoherentInelastic() each, but
 * uses the same approach of approximating the individual inelastic parts by the incoherent cross
 * section.
 * This approach works sufficiently for thermal neutrons as shown by Binder (1970), but is actually
 * rather weak for the cold neutron region.
 *
 * References:
 *  - K. Binder (1970) Phys. Stat. Sol. 41, 767-779.
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>tot_inel_BINDER</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
double nxs_TotalInelastic_BINDER( double lambda, NXS_UnitCell* uc )
{
  unsigned int i;
  double phi1_phi3, value, s_el_inc, A,  s_total_inc;
  double xsect_total_inel = 0.0;

  for( i=0; i<uc->nAtomInfo; i++ )
  {
    phi1_phi3 = uc->atomInfoList[i].phi_1 * uc->atomInfoList[i].phi_3;
    value = lambda*lambda / 2.0 / uc->atomInfoList[i].B_iso;
    s_el_inc = value * ( 1.0 - exp(-1.0/value) );
    A = uc->atomInfoList[i].M_m;
    s_total_inc = A/(A+1.0)*A/(A+1.0) * ( 1.0 + 9.0 * phi1_phi3 * value/A/A );
    xsect_total_inel += (s_total_inc - s_el_inc) * uc->atomInfoList[i].nAtoms;
  }

  return xsect_total_inel * (uc->avgSigmaCoherent+uc->avgSigmaIncoherent);
}


/**
 * \fn double nxs_TotalInelastic_COMBINED( double lambda, NXS_UnitCell* uc )
 * \brief Calculates the total inelastic neutron cross section as a combination of two approaches.
 *
 * This function calculates the total inelastic scattering cross section
 * as a combination of two different approaches by returning nxs_SinglePhonon() + nxs_MultiPhonon_COMBINED().
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>tot_inel_COMBINED</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
double nxs_TotalInelastic_COMBINED( double lambda, NXS_UnitCell* uc )
{
  return nxs_SinglePhonon( lambda, uc ) + nxs_MultiPhonon_COMBINED( lambda, uc );
}


/**
 * \fn double nxs_SinglePhonon( double lambda, NXS_UnitCell* uc )
 * \brief Calculates the single phonon part of the total inelastic neutron cross section.
 *
 * This function calculates single phonon part of the total inelastic scattering cross section
 * per unit cell after Freund (1983).
 *
 * References:
 *  - A.K. Freund (1983) Nucl. Instr. Meth. 213, 495-501
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>sph</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
double nxs_SinglePhonon( double lambda, NXS_UnitCell* uc )
{
  unsigned int i = 0;

  // Angstrom to eV
  // double neutronEnergy = PLANCK_CONSTANT_eVs*PLANCK_CONSTANT_eVs/(2*lambda*lambda*1E-20*MASS_NEUTRON_kg * J2EV);
  double neutronEnergy = 8.18042531017E-2 / lambda / lambda;

  double sph = 0.0;
  for( i=0; i<uc->nAtomInfo; i++ )
    sph += uc->atomInfoList[i].sph * uc->atomInfoList[i].nAtoms;

  return sph * (uc->avgSigmaCoherent+uc->avgSigmaIncoherent) / 35.90806936252971 / sqrt(neutronEnergy);
}


/**
 * \fn double nxs_MultiPhonon( double lambda, NXS_UnitCell* uc )
 * This function is provided for convenience and to be downward compatible to earlier versions of the nxs library.
 * It currently links to nxs_MultiPhonon_COMBINED().
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>mph</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
double nxs_MultiPhonon( double lambda, NXS_UnitCell* uc )
{
  return nxs_MultiPhonon_COMBINED( lambda, uc );
}


/**
 * \fn double nxs_MultiPhonon_CASSELS( double lambda, NXS_UnitCell* uc )
 * \brief Calculates the multiple phonon part of the total inelastic neutron cross section.
 *
 * This function calculates multiple phonon part of the total inelastic scattering cross section
 * per unit cell after Cassels (1950).
 *
 * References:
 *  - J.M. Cassels (1950) Prog. Nucl. Phys. 1, 185
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>mph_CASSELS</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
double nxs_MultiPhonon_CASSELS( double lambda, NXS_UnitCell* uc )
{
  unsigned int i = 0;
  double mph = 0.0;
  for( i=0; i<uc->nAtomInfo; i++ )
  {
    double value = lambda*lambda / 2.0 / uc->atomInfoList[i].B_iso;
    double s_el_inc = value * ( 1.0 - exp(-1.0/value) );
    double A = uc->atomInfoList[i].M_m;
    mph += A/(A+1.0)*A/(A+1.0) * (1 - s_el_inc) * uc->atomInfoList[i].nAtoms;
  }
  return mph *= (uc->avgSigmaCoherent+uc->avgSigmaIncoherent);
}


// /* implementation of phi(x) by Th. Kittelmann after A.K. Freund (1983) Nucl. Instr. Meth. 213, 495-501 */
//static double phi_integrand(double s)
//{
//  //evaluate s/(exp(s)-1) with correct limit at s->0 to at least ?? significant figures
//  if (s>0.05||s<-0.05)
//    return s/(exp(s)-1);

//  return 1-0.5*s+s*s*(1.0/12.0)-s*s*s*s*(1.0/720)+s*s*s*s*s*s*(1.0/30240.0)-s*s*s*s*s*s*s*s*(1.0/1209600.);
//  //Refactored slightly faster:
//  //double s2=s*s;
//  //return 1.0+s*(-0.5+s*((1/12.0)+s2*((-1.0/720)+s2*((1.0/30240.0)-s2*(1.0/1209600.)))));
//}

//static double phi(double x)
//{
//  //Numerical integration of s/(exp(-s)-1) from 0 to x, divided by x. Does not
//  //need to be particularly fast as it is evaluated only once per material, so
//  //we use a simple application of integration via Simpson's rule (could easily
//  //be optimised further):
//  const unsigned n = 10000;
//  const double dx = x / n;
//  double sum = 0;
//  unsigned int i;

//  if (x==0)
//    return 1;//limiting value

//  for ( i=0; i<n; ++i)
//    sum += phi_integrand(i*dx)+4*phi_integrand((i+0.5)*dx)+phi_integrand((i+1)*dx);

//  return sum * dx/( 6.0 * x );//notice the minus to get the correct form
//}

/**
 * \fn double nxs_MultiPhonon_FREUND( double lambda, NXS_UnitCell* uc )
 * \brief Calculates the multiple phonon part of the total inelastic neutron cross section.
 *
 * This function calculates multiple phonon part of the total inelastic scattering cross section
 * per unit cell after Freund (1983).
 *
 * References:
 *  - A.K. Freund (1983) Nucl. Instr. Meth. 213, 495-501
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>mph_FREUND</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
double nxs_MultiPhonon_FREUND( double lambda, NXS_UnitCell* uc )
{
  // calculation of BT an B0 is not neccessary here, as BT + B0 = Biso
  // hence, the phi(x) calculation is obsolete

  unsigned int i = 0;
  double mph = 0.0;

  double C2_times_Energy = uc->mph_c2 *  8.18042531017E-2/lambda/lambda;
  for( i=0; i<uc->nAtomInfo; i++ )
  {
    double A = uc->atomInfoList[i].M_m;

    /* BT+B0=Biso, i.e. no need to calculate BT and B0 here */
    // double B0 = 2872.568576905348 / (A*uc->atomInfoList[i].debyeTemp);
    // double x = uc->atomInfoList[i].debyeTemp / uc->temperature;
    // double BT = 4.0 * B0 * phi(x) / x;

    double mphPerAtom = A*A / ( (1.0+A)*(1.0+A) ) * ( 1.0 - exp( -uc->atomInfoList[i].B_iso * C2_times_Energy) );
    mph += mphPerAtom * uc->atomInfoList[i].nAtoms;
  }
  return mph * (uc->avgSigmaCoherent+uc->avgSigmaIncoherent);
}


/**
 * \fn double nxs_MultiPhonon_COMBINED( double lambda, NXS_UnitCell* uc )
 * \brief Calculates the total inelastic neutron cross section as a combination of two approaches.
 *
 * This function calculates the multiple phone part of the total inelastic scattering cross section
 * as a combination of two different approaches. For neutron with $E > k_B \theta_D$ an incoherent
 * approximation for the inelastic scattering part (Cassels[1950]) is used. For $E < k_B \theta_D$ Freund's (1983)
 * multiple phonon cross section calculation is applied.
 * Here, a linear switchover in the region around $E = k_B \theta_D$ is computed between nxs_MultiPhonon_CASSELS() and
 * nxs_MultiPhonon_FREUND(). This approach was suggested by Adib (2007), for example.
 *
 * References:
 *  - A.K. Freund (1983) Nucl. Instr. Meth. 213, 495-501
 *  - J.M. Cassels (1950) Prog. Nucl. Phys. 1, 185
 *  - M. Adib & M. Fathalla (2008) Egyptian Nuclear Physics Association (ENPA), Egyptian Atomic Energy Authority (EAEA), 642 p, 39-54,
 *  NUPPAC 07; 6th Conference on Nuclear and Particle Physics; Luxor (Egypt); 17-21 Nov 2007
 *
 * @param lambda wavelength in &Aring;
 * @param uc NXS_UnitCell struct
 * @return &sigma;<sub>mph_COMBINED</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
double nxs_MultiPhonon_COMBINED( double lambda, NXS_UnitCell* uc )
{
  double lambda_debye = 30.8106673293723 / sqrt( uc->debyeTemp );

  /* these limits are estimates from the graphite case in Adib (2007) to provide a universal approach (for all materials) */
  double lambda_Cassels = lambda_debye * 1.78789683887;
  double lambda_Freund = lambda_debye * 3.68096408002;

  if( lambda>=lambda_Freund )
    return nxs_MultiPhonon_FREUND( lambda, uc );
  else if( lambda<=lambda_Cassels )
    return nxs_MultiPhonon_CASSELS( lambda, uc );
  else
  {
    /* this is the linear switchover proposed by by Adib (2007) */
    return (lambda_Freund-lambda)/(lambda_Freund-lambda_Cassels)*nxs_MultiPhonon_CASSELS( lambda, uc ) + (lambda-lambda_Cassels)/(lambda_Freund-lambda_Cassels)*nxs_MultiPhonon_FREUND( lambda, uc );
  }
}


/**
 * \fn NXS_MarchDollase nxs_initMarchDollase( NXS_UnitCell* uc )
 * \brief Initialises a NXS_MarchDollase struct.
 *
 * @param uc NXS_UnitCell struct
 * @return NXS_MarchDollase struct
 */
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


/**
 * \fn void nxs_addTexture( NXS_MarchDollase* md, NXS_Texture texture )
 * \brief Adds texture to the unit cell via the NXS_MarchDollase struct.
 *
 * The function adds a preferred orientation (a,b,c) with weighting factors r and f, stored in texture,
 * to the unit cell via md and prepares for the March-Dollase texture calculation.
 * @param md NXS_MarchDollase struct
 * @param texture NXS_Texture struct
 */
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


/**
 * \fn double nxs_CoherentElasticTexture( double lambda, NXS_MarchDollase* md )
 * \brief Calculates the coherent elastic scattering cross section with texture influence.
 *
 * The function computes the coherent elastic scattering cross section but also applies a
 * March-Dollase texture correction. The unit cell information must be stored to the
 * NXS_MarchDollase struct.
 * @param lambda wavelength in &Aring;
 * @param md NXS_MarchDollase struct
 * @return &sigma;<sub>coh_el_texture</sub> [barn = 10<sup>-24</sup> cm<sup>2</sup>]
 */
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


/* keys for the nxs parameter file */
const char *NXS_keys[] =
{
  "space_group",
  "lattice_a",
  "lattice_b",
  "lattice_c",
  "lattice_alpha",
  "lattice_beta",
  "lattice_gamma",
  "add_atom",
  "mph_c2",
  "debye_temp"
};


/**
 * \fn int nxs_readParameterFile( const char* fileName, NXS_UnitCell *uc, NXS_AtomInfo *atomInfoList[] )
 * \brief Reads a nxs parameter file.
 *
 * The function reads the nxs parameter file fileName and stores the data to NXS_UnitCell and the atom
 * information to atomInfoList.
 * In case of a successful read the funtion returns the number of atom info line read from the file.
 * Otherwise, an error code is returned. Then, the funtion nxs_initUnitCell() should be called after this.
 * @param fileName
 * @param uc NXS_UnitCell struct
 * @param atomInfoList NXS_AtomInfo struct
 * @return nxs error code
 */
int nxs_readParameterFile( const char* fileName, NXS_UnitCell *uc, NXS_AtomInfo *atomInfoList[] )
{
  unsigned int numAtoms = 0;
  NXS_AtomInfo ai;
  NXS_AtomInfo *aiList = NULL;

  char line[200];
  unsigned int max_keys = sizeof(NXS_keys)/sizeof(NXS_keys[0]);

  /* open the parameter file */
  FILE* file;
  file = fopen(fileName, "r");

  *uc = nxs_newUnitCell();

  if (!file)
  {
    return NXS_ERROR_READINGFILE;
  }

  /* to make sure parameters are initialized if not in the parameter file */

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
        unsigned int nwords = 0;
        char isinword = 0;

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
            /* first count the words in that line */
            endptr = par;
            nwords = 0;
            for( ; *endptr; ++endptr)
            {
              if( !isspace(*endptr) )
              {
                if( isinword == 0 )
                {
                    isinword = 1;
                    ++nwords;
                }
              }
              else if( isinword )
                isinword = 0;
            }
            /* check number of words */
            if( nwords<8 )
              return NXS_ERROR_READINGFILE;
            else
            {
              strncpy(ai.label,strtok(par, " \t"),MAX_CHARS_ATOMLABEL);     /* atom name or label */
              ai.b_coherent = strtod(strtok( NULL, " \t" ), &endptr);       /* b_coh */
              ai.sigmaIncoherent = strtod(strtok( NULL, " \t" ), &endptr);  /* sigma_inc */
              ai.sigmaAbsorption = strtod(strtok( NULL, " \t" ), &endptr);  /* sigma_abs_2200 */
              ai.molarMass = strtod(strtok( NULL, " \t" ), &endptr);        /* molar_mass */

              /* for compatibility to earlier versions of nxs the atom specific entry of a */
              /* Debye temperature is also allowed. If not given, -1.0 is set and continue with x y z */
              if( nwords>8 )
                ai.debyeTemp = strtod(strtok( NULL, " \t" ), &endptr);      /* debye_temp */
              else
                ai.debyeTemp = -1.0;

              /* the Wyckoff postion of the atom inside the unit cell */
              ai.x[0] = strtod(strtok( NULL, " \t" ), &endptr);             /* x */
              ai.y[0] = strtod(strtok( NULL, " \t" ), &endptr);             /* y */
              ai.z[0] = strtod(strtok( NULL, " \t" ), &endptr);             /* z */

              /* collect all "add_atom" entries from file to make sure     */
              /* uc is initialised first before nxs_addAtomInfo is applied */
              numAtoms++;
              aiList = (NXS_AtomInfo*)realloc( aiList, sizeof(NXS_AtomInfo)*numAtoms );
              aiList[numAtoms-1] = ai;
            }
            break;
          case 8:                                    /* mph_c2 */
            uc->mph_c2 = strtod(par, &endptr);
            break;
          case 9:                                    /* global debye_temp */
            uc->debyeTemp = strtod(par, &endptr); break;
          default:  break;

        } /* end of: switch( i ) */
      } /* end of: if( i < max_keys ) */
    } /* end of: if( par != NULL ) */
  } /* end of: while( fgets(line, sizeof(line), file) != NULL ) */

  /* finished with reading file */
  fclose(file);
  if( !numAtoms )
    return NXS_ERROR_NOATOMINFOFOUND;

  *atomInfoList = aiList;
  return numAtoms;
}


/**
 * \fn int nxs_saveParameterFile( const char* fileName, NXS_UnitCell *uc )
 * \brief Saves the current unit cell settings to a nxs parameter file.
 *
 * @param fileName
 * @param uc NXS_UnitCell struct
 * @return nxs error code
 */
int nxs_saveParameterFile( const char* fileName, NXS_UnitCell *uc )
{
  unsigned int i = 0;

  /* open the parameter file */
  FILE* file;
  file = fopen(fileName, "w");

  if (!file)
  {
    return NXS_ERROR_SAVINGFILE;
  }

  fprintf( file, "#\n# This is an nxs parameter file\n#\n%s = %s\n%s = %f\n%s = %f\n%s = %f\n%s = %f\n%s = %f\n%s = %f\n%s = %f\n%s = %f\n\n# label  b_coherent  sigma_inc  sigma_abs  molar_mass  debye_temp  x  y  z\n",
        NXS_keys[0], uc->spaceGroup, NXS_keys[1], uc->a, NXS_keys[2], uc->b, NXS_keys[3], uc->c, NXS_keys[4], uc->alpha, NXS_keys[5], uc->beta, NXS_keys[6], uc->gamma, NXS_keys[8], uc->mph_c2, NXS_keys[9], uc->debyeTemp);

  for(i=0; i<uc->nAtomInfo; i++)
  {
    fprintf( file, "%s = %s %f %f %f %f ",
         NXS_keys[7], uc->atomInfoList[i].label, uc->atomInfoList[i].b_coherent, uc->atomInfoList[i].sigmaIncoherent,
         uc->atomInfoList[i].sigmaAbsorption, uc->atomInfoList[i].molarMass );

    /* if debye temp value is stored to atomInfo store it */
    /* this is maybe removed in the future */
    if( uc->atomInfoList[i].debyeTemp<1E-6 )
      fprintf( file, "%f ", uc->atomInfoList[i].debyeTemp );

    fprintf( file, "%f %f %f\n", uc->atomInfoList[i].x[0], uc->atomInfoList[i].y[0], uc->atomInfoList[i].z[0] );
  }

  fclose(file);

  return NXS_ERROR_OK;
}


/**
 * \fn const char* nxs_version()
 * \brief Returns the library version number.
 *
 * @return const char*
 */
const char* nxs_version()
{
#ifdef NXSLIB_VERSION
  #define VERNUM2STR(x) VERSTR(x)
  #define VERSTR(x) #x
  #define RETURNVALUE VERNUM2STR( NXSLIB_VERSION )
#else
  #define RETURNVALUE (const char*)"unknown"
#endif

  return RETURNVALUE;
}


/**
 * \fn NXS_UnitCell nxs_newUnitCell()
 * \brief Returns an empty but correctly initialized unit cell.
 *
 * This funtion is called by nxs_readParameterFile(). So, there is only need to call this function if the individual
 * unit cell parameters will be assigned manually, i.e. without the file reading. Like for nxs_readParameterFile(), after
 * the assigning the unit cell parameters the funtion nxs_initUnitCell() should be called.
 *
 * @return NXS_UnitCell
 */
NXS_UnitCell nxs_newUnitCell()
{
  NXS_UnitCell uc;
  uc.spaceGroup[0] = '\0';
  uc.a = 0.0;
  uc.b = 0.0;
  uc.c = 0.0;
  uc.alpha = 0.0;
  uc.beta = 0.0;
  uc.gamma = 0.0;
  /* actual values should never be smaller than 0, hence a -1.0 is set as a start value */
  uc.mph_c2 = -1.0;  // if smaller 0 then mph_c2 is computed during nxs_addAtomInfo()
  uc.debyeTemp = -1.0; // if smaller 0 then debyeTemp is set during nxs_addAtomInfo()

  uc.crystalSystem = XS_Unknown;
  uc.avgSigmaCoherent = 0.0;
  uc.avgSigmaIncoherent = 0.0;
  uc.nAtoms = 0;
  uc.nAtomInfo = 0;
  uc.atomInfoList = 0;
  uc.temperature = 293.0;
  uc.volume = 0.0;
  uc.mass = 0.0;
  uc.density = 0.0;
  uc.nHKL = 0;
  uc.maxHKL_index = 0;
  uc.hklList = 0;
  uc.alpha = 0.0;

  uc.__flag_mph_c2 = 1;

  return uc;
}


/**
 * \fn nxs_initFromParameterFile(const char *fileName, NXS_UnitCell *uc)
 * \brief Initializes a unit cell and the hkl lattice reflections from reading a parameter file.
 *
 * This function is provided for convenience to ensure a safe subsequent call of nxs_readParameterFile(), nxs_initUnitCell(),
 * nxs_addAtomInfo() and nxs_initHKL() if all necessary parameters are given in the parameter file. After calling this
 * function the unit cell is prepared for neuron cross section calculations.
 *
 * @param fileName
 * @param uc NXS_UnitCell struct
 * @return nxs error code
 */
int nxs_initFromParameterFile(const char *fileName, NXS_UnitCell *uc)
{
  int numAtoms = 0;
  NXS_AtomInfo *atomInfoList;
  numAtoms = nxs_readParameterFile( fileName, uc, &atomInfoList);

  if( numAtoms < 1 )
  {
    return numAtoms;
  }
  else
  {
    int error = nxs_initUnitCell( uc );
    if( NXS_ERROR_OK != error )
    {
      return error;
    }
    else
    {
      int i;
      for( i=0; i<numAtoms; i++ )
      {
        error = nxs_addAtomInfo( uc, atomInfoList[i] );
        if( NXS_ERROR_OK != error )
          return error;
      }
      return nxs_initHKL( uc );
    }
  }
}
