/**
    nxs - neutron cross sections (c) 2010-2014 Mirko Boin
*/

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef NXS_H
#define NXS_H

#include <stdio.h>

#if defined(__THINK__) || defined(__MWERKS__)
#include <console.h>
#define CONSOLE_LINES   36  /* number of lines to use for console */
#define CONSOLE_COLUMNS 90  /* number of columns to use for console */
#ifdef __MWERKS__
#include <sioux.h>
#endif
#endif

#define AppMalloc(ptr, n) (ptr) = malloc((n) * sizeof (*(ptr)))
#define AppFree(ptr, n) free(ptr)

#define SGCOREDEF__
#include "sginfo.h"


#define NXSLIB_VERSION 1.5


/**
    @author Mirko Boin, Helmholtz-Zentrum Berlin f&uuml; Materialien und Energy GmbH, <boin@helmholtz-berlin.de>
*/


/*!
    Details:

    The nxs library for computing neutron scattering and absorption cross sections provides
    a number of C structs and functions to calculate wavelength-dependent cross section values for
    polycrystalline/powder-like materials. The definition of a material is represented by the composition
    of a unit cell (NXS_UnitCell). A unit cell is created from the specification of a space group and its
    unit cell parameters. The SgInfo routines from Ralf W. Grosse-Kunstleve is included here for such
    purposes. Monoatomic materials as well as multi-atomic compounds are created by adding NXS_AtomInfo
    atom information/properties. The library also provides a reading and saving routines to compose unit
    cells from nxs parameter files.


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

    \copyright
    nxs - neutron cross sections (c) 2010-2014 Mirko Boin

    The nxs library includes the SgInfo library, whose free usage is granted by the following notice:

    Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve
    Permission to use and distribute this software and its documentation for noncommercial
    use and without fee is hereby granted, provided that the above copyright notice appears
    in all copies and that both that copyright notice and this permission notice appear in
    the supporting documentation. It is not allowed to sell this software in any way. This
    software is not in the public domain.
*/

#define MAX_CHARS_SPACEGROUP 40
#define MAX_CHARS_ATOMLABEL 100


// nxs error codes
#define NXS_ERROR_OK                      0
#define NXS_ERROR_NOMATCHINGSPACEGROUP   -1
#define NXS_ERROR_NOATOMINFOFOUND        -2
#define NXS_ERROR_READINGFILE            -10
#define NXS_ERROR_SAVINGFILE             -11
#define NXS_ERROR_MEMORYALLOCATIONFAILED -20


/*!
 \fn nxs_version
 \return
*/
const char* nxs_version();



/************************ UNIT CELL structs/functions ************************/
/**
\struct <NXS_EquivHKL>

  \brief struct for symmetry equivalent Miller indices (hkl)

  This struct is used by struct NXS_HKL to hold symmetry equivalent hkl.
  \see NXS_HKL
*/
typedef struct NXS_EquivHKL {
  int h;                           /*!< Miller index */
  int k;                           /*!< Miller index */
  int l;                           /*!< Miller index */
} NXS_EquivHKL;


/**
\struct <NXS_HKL>

  \brief struct for Miller indices (hkl)

  According to a given reflection this struct stores the hkl indices, its multiplicity, its lattice spacing, its structure factor and the symmetry equivalent hkl indices.
*/
typedef struct NXS_HKL {
  int h;                           /*!< Miller index */
  int k;                           /*!< Miller index */
  int l;                           /*!< Miller index */
  unsigned int multiplicity;       /*!< multiplicity of the hkl reflection */
  double dhkl;                     /*!< hkl lattice spacing in &Aring;*/
  double FSquare;                  /*!< \f$|F|^2\f$ (structure factor) */
  NXS_EquivHKL *equivHKL;          /*!< holds the symmetry equivalent reflections including the current indices */
} NXS_HKL;


/**
\struct <NXS_AtomInfo>

  \brief struct for atom descrpition

  This struct stores the position of an atom, its average cross sections, mass and Debye temperature.
*/
typedef struct NXS_AtomInfo {
  char wyckoffLetter;               /*!< Wyckoff letter */
  unsigned int nAtoms;              /*!< number of atoms = Wyckoff multiplicity */
  unsigned int elementNumber;       /*!< Element number from PSE */
  char label[MAX_CHARS_ATOMLABEL];  /*!< Label (name) for the atom */
  double x[192];          /*!< holds the x positions, the first entry is the Wyckoff position */
  double y[192];          /*!< holds the y positions, the first entry is the Wyckoff position */
  double z[192];          /*!< holds the z positions, the first entry is the Wyckoff position */
  double sigmaAbsorption; /*!< in [\f$barn = 10^{-24} cm^2  at \ 2200 \frac{m}{s} = 1.798 \AA\f$] */
  double sigmaCoherent;   /*!< in [\f$barn = 10^{-24} cm^2\f$] */
  double sigmaIncoherent; /*!< in [\f$barn = 10^{-24} cm^2\f$] */
  double b_coherent;      /*!< in [fm] */
  double molarMass;       /*!< Molar mass in [\f$u = 1.66 \times 10^{-27} kg\f$] */
  double M_m;             /*! M/m = molarMass*ATOMIC_MASS_U_kg/MASS_NEUTRON_kg  / */
  double debyeTemp;       /*!< Debye temperature in [K] */
  double phi_1;           /*!< \f$\varphi_1\f$ */
  double phi_3;           /*!< \f$\varphi_3\f$ */
  double B_iso;           /*!< \f$B_{iso}\f$ the isotropic atomic displacement factor */
  double sph;             /*!< Single phonon part per atom info */
} NXS_AtomInfo;


/**
\struct <NXS_UnitCell>

  \brief struct for unit cell description

  This struct stores space group symbol, the lattice parameters, the three sigma values, the atom mass the Debye temperature, some calculated values and the HKL and UnitCell struct as well as the SgInfo struct (see SgInfo documentation on http://cci.lbl.gov/sginfo/).
*/
typedef struct NXS_UnitCell {
  int crystalSystem;                     /*!< crysal system: XS_Tetragonal, XS_Hexagonal, XS_Cubic ... */
  char spaceGroup[MAX_CHARS_SPACEGROUP]; /*!< space group number or denotation */
  double a;                              /*!< lattice constant a */
  double b;                              /*!< lattice constant b */
  double c;                              /*!< lattice constant c */
  double alpha;                          /*!< lattice constant \f$\alpha\f$ */
  double beta;                           /*!< lattice constant \f$\beta\f$ */
  double gamma;                          /*!< lattice constant \f$\gamma\f$ */
  double mph_c2;                         /*!< constant for mph calculation from A.K. Freund (1983) Nucl. Instr. Meth. 213, 495-501, if not defined C2 = 4.27*exp( A/61.0 ) will be calculated */
  double debyeTemp;                      /*!< Debye temperature in [K] */
  double avgSigmaCoherent;               /*!< in [\f$barn = 10^{-24} cm^2\f$] */
  double avgSigmaIncoherent;             /*!< in [\f$barn = 10^{-24} cm^2\f$] */
  unsigned int nAtoms;                   /*!< total number of atoms inside the unit cell */
  unsigned int nAtomInfo;                /*!< number of unit cell atoms */
  NXS_AtomInfo *atomInfoList;            /*!< atom info \see AtomInfo */
  T_SgInfo sgInfo;                       /*!< struct from SgInfo library needed for further calculations see SgInfo documentation on http://cci.lbl.gov/sginfo/ */
  double temperature;                    /*!< sample environment temperature [K] */
  double volume;                         /*!< unit cell volume */
  double mass;                           /*!< unit cell mass [\f$\frac{g}{mol}\f$]*/
  double density;                        /*!< unit cell density [\f$\frac{g}{cm^3}\f$]*/
  unsigned int nHKL;                     /*!< number of hkl reflections after initUnitCell() */
  unsigned int maxHKL_index;             /*!< maximum hkl index */
  NXS_HKL *hklList;                      /*!< \see NXS_HKL */
  unsigned char __flag_mph_c2;           /*!< flag to indicate if mph_c2 is set or should be calculated */
} NXS_UnitCell;


NXS_UnitCell nxs_newUnitCell();
int nxs_initUnitCell( NXS_UnitCell *uc );
int nxs_addAtomInfo( NXS_UnitCell *uc, NXS_AtomInfo ai );
int nxs_initHKL( NXS_UnitCell *uc );
double nxs_calcDhkl( int h, int k, int l, NXS_UnitCell *uc );
double nxs_calcFSquare( NXS_HKL *hklReflex, NXS_UnitCell *uc );
/*****************************************************************************/



/************************** CROSS SECTION FUNCTIONS **************************/
double nxs_Absorption             ( double lambda, NXS_UnitCell* uc );
double nxs_CoherentElastic        ( double lambda, NXS_UnitCell* uc );
double nxs_CoherentInelastic      ( double lambda, NXS_UnitCell* uc );
double nxs_TotalInelastic         ( double lambda, NXS_UnitCell* uc );
double nxs_TotalInelastic_BINDER  ( double lambda, NXS_UnitCell* uc );
double nxs_TotalInelastic_COMBINED( double lambda, NXS_UnitCell* uc );
double nxs_SinglePhonon           ( double lambda, NXS_UnitCell* uc );
double nxs_MultiPhonon            ( double lambda, NXS_UnitCell* uc );
double nxs_MultiPhonon_CASSELS    ( double lambda, NXS_UnitCell* uc );
double nxs_MultiPhonon_FREUND     ( double lambda, NXS_UnitCell* uc );
double nxs_MultiPhonon_COMBINED   ( double lambda, NXS_UnitCell* uc );
double nxs_IncoherentElastic      ( double lambda, NXS_UnitCell* uc );
double nxs_IncoherentInelastic    ( double lambda, NXS_UnitCell* uc );
/*****************************************************************************/



/******************************* TEXTURE STUFF *******************************/
/**
\struct <NXS_Texture>

  \brief struct for texture descrpition using March-Dollase approach

*/
typedef struct NXS_Texture {
  int a;
  int b;
  int c;
  double r;
  double f;
  double **sin_beta;
  double **cos_beta;
  double *P_alpha_H;
} NXS_Texture;


/**
\struct <NXS_MarchDollase>

  \brief struct for March-Dollase correction calculation

*/
typedef struct NXS_MarchDollase {
  unsigned int M;
  unsigned int N;
  unsigned int nOrientations;
  double *sin_phi;
  double *cos_phi;
  NXS_Texture *texture;
  NXS_UnitCell *unitcell;
} NXS_MarchDollase;

NXS_MarchDollase nxs_initMarchDollase( NXS_UnitCell* uc );
void nxs_addTexture( NXS_MarchDollase* md, NXS_Texture texture );
double nxs_CoherentElasticTexture( double lambda, NXS_MarchDollase* md );
/*****************************************************************************/



/************************** PARAMETER FILE ROUTINES **************************/
int nxs_readParameterFile( const char* fileName, NXS_UnitCell *uc , NXS_AtomInfo *atomInfoList[] );
int nxs_saveParameterFile( const char* fileName, NXS_UnitCell *uc );
int nxs_initFromParameterFile( const char* fileName, NXS_UnitCell *uc );
/*****************************************************************************/



#endif

#ifdef __cplusplus
}
#endif
