/*
  Space Group Info's (c) 1994-96 Ralf W. Grosse-Kunstleve
 */

#ifndef SGINFO_H__
#define SGINFO_H__


#ifndef SGCLIB_C__
extern
const char *SgError;
#ifdef SGCOREDEF__
extern
char        SgErrorBuffer[128];
#endif
#else
const char *SgError = NULL;
char        SgErrorBuffer[128];
#endif


#define STBF 12 /* Seitz           Matrix Translation Base Factor */

#define CRBF 12 /* Change of Basis Matrix Rotation    Base Factor */
#define CTBF 72 /* Change of Basis Matrix Translation Base Factor */

/* CAUTION: (CTBF / STBF) has to be an INTEGER */


typedef struct
  {
    int  Code;
    int  nTrVector;
    int  *TrVector;
  }
  T_LatticeInfo;


typedef union
  {
    struct { int R[9], T[3]; } s;
    int                        a[12];
  }
  T_RTMx;


typedef struct
  {
    int  EigenVector[3];
    int  Order;
    int  Inverse;
    int  RefAxis;
    int  DirCode;
  }
  T_RotMxInfo;


typedef struct
  {
    const char  *HallSymbol;
    int         SgNumber;
    const char  *Extension;
    const char  *SgLabels;
  }
  T_TabSgName;


#define MaxLenHallSymbol  39


typedef struct
  {
    int                  GenOption;
    int                  Centric;
    int                  InversionOffOrigin;
    const T_LatticeInfo  *LatticeInfo;
    int                  StatusLatticeTr;
    int                  OriginShift[3];
    int                  nList;
    int                  MaxList;
    T_RTMx               *ListSeitzMx;
    T_RotMxInfo          *ListRotMxInfo;
    int                  OrderL;
    int                  OrderP;
    int                  XtalSystem;
    int                  UniqueRefAxis;
    int                  UniqueDirCode;
    int                  ExtraInfo;
    int                  PointGroup;
    int                  nGenerator;
    int                   Generator_iList[4];
    char                 HallSymbol[MaxLenHallSymbol + 1];
    const T_TabSgName    *TabSgName;
    const int            *CCMx_LP;
    int                  n_si_Vector;
    int                  si_Vector[9];
    int                  si_Modulus[3];
  }
  T_SgInfo;

/* T_Sginfo.GenOption:  0 = full group generation
                        1 = trusted:
                            set Centric/InversionOffOrigin/LatticeInfo only
                       -1 = no group generation

   T_Sginfo.Centric:   0 = acentric
                       1 = inversion in list
                      -1 = inversion removed from list

   T_Sginfo.StatusLatticeTr:   0 = removed from list
                               1 = all translation vectors in list
                              -1 = some translation vectors could be
                                   missing in list
 */


typedef struct
  {
    int  M;      /* Multiplicity */
    int  N;      /* Number of equivalent hkl to follow */
    int  h[24];  /* If hkl == 000 M = N = 1 */
    int  k[24];  /* If hkl != 000 M = 2 * N */
    int  l[24];  /* List of hkl does not contain friedel mates */
    int  TH[24]; /* Phase shift relative to h[0], k[0], l[0] */
  }
  T_Eq_hkl;


#define EI_Unknown         0
#define EI_Enantiomorphic  1
#define EI_Obverse         2
#define EI_Reverse         3

#ifndef SGCLIB_C__
extern
const char *EI_Name[];
#else
const char *EI_Name[] =
  {
    "Unknown",
    "Enantiomorphic",
    "Obverse",
    "Reverse"
  };
#endif


#define XS_Unknown       0
#define XS_Triclinic     1
#define XS_Monoclinic    2
#define XS_Orthorhombic  3
#define XS_Tetragonal    4
#define XS_Trigonal      5
#define XS_Hexagonal     6
#define XS_Cubic         7

#ifndef SGCLIB_C__
extern
const char *XS_Name[];
#else
const char *XS_Name[] =
  {
    "Unknown",
    "Triclinic",
    "Monoclinic",
    "Orthorhombic",
    "Tetragonal",
    "Trigonal",
    "Hexagonal",
    "Cubic"
  };
#endif


#define             Make_PG_Code( i,  p,  l) (((i) * 33 + (p)) * 12 + (l))
#define PG_Unknown  Make_PG_Code( 0,  0,  0)
#define PG_1        Make_PG_Code( 1,  1,  1)
#define PG_1b       Make_PG_Code( 2,  2,  1)
#define PG_2        Make_PG_Code( 3,  3,  2)
#define PG_m        Make_PG_Code( 4,  4,  2)
#define PG_2_m      Make_PG_Code( 5,  5,  2)
#define PG_222      Make_PG_Code( 6,  6,  3)
#define PG_mm2      Make_PG_Code( 7,  7,  3)
#define PG_mmm      Make_PG_Code( 8,  8,  3)
#define PG_4        Make_PG_Code( 9,  9,  4)
#define PG_4b       Make_PG_Code(10, 10,  4)
#define PG_4_m      Make_PG_Code(11, 11,  4)
#define PG_422      Make_PG_Code(12, 12,  5)
#define PG_4mm      Make_PG_Code(13, 13,  5)
#define PG_4b2m     Make_PG_Code(14, 14,  5)
#define PG_4bm2     Make_PG_Code(15, 14,  5)
#define PG_4_mmm    Make_PG_Code(16, 15,  5)
#define PG_3        Make_PG_Code(17, 16,  6)
#define PG_3b       Make_PG_Code(18, 17,  6)
#define PG_321      Make_PG_Code(19, 18,  7)
#define PG_312      Make_PG_Code(20, 18,  7)
#define PG_32       Make_PG_Code(21, 18,  7)
#define PG_3m1      Make_PG_Code(22, 19,  7)
#define PG_31m      Make_PG_Code(23, 19,  7)
#define PG_3m       Make_PG_Code(24, 19,  7)
#define PG_3bm1     Make_PG_Code(25, 20,  7)
#define PG_3b1m     Make_PG_Code(26, 20,  7)
#define PG_3bm      Make_PG_Code(27, 20,  7)
#define PG_6        Make_PG_Code(28, 21,  8)
#define PG_6b       Make_PG_Code(29, 22,  8)
#define PG_6_m      Make_PG_Code(30, 23,  8)
#define PG_622      Make_PG_Code(31, 24,  9)
#define PG_6mm      Make_PG_Code(32, 25,  9)
#define PG_6bm2     Make_PG_Code(33, 26,  9)
#define PG_6b2m     Make_PG_Code(34, 26,  9)
#define PG_6_mmm    Make_PG_Code(35, 27,  9)
#define PG_23       Make_PG_Code(36, 28, 10)
#define PG_m3b      Make_PG_Code(37, 29, 10)
#define PG_432      Make_PG_Code(38, 30, 11)
#define PG_4b3m     Make_PG_Code(39, 31, 11)
#define PG_m3bm     Make_PG_Code(40, 32, 11)

#define PG_Index(PG_Code)   ((PG_Code) / (33 * 12))
#define PG_Number(PG_Code) (((PG_Code) / 12) % 33)
#define LG_Number(PG_Code)  ((PG_Code) % (33 * 12))

#ifndef SGCLIB_C__
extern
const int LG_Code_of_PG_Index[];
#else
const int LG_Code_of_PG_Index[] =
  {
    PG_Unknown,
    PG_1b,
    PG_1b,
    PG_2_m,
    PG_2_m,
    PG_2_m,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_4_m,
    PG_4_m,
    PG_4_m,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_3b,
    PG_3b,
    PG_3bm1,
    PG_3b1m,
    PG_3bm,
    PG_3bm1,
    PG_3b1m,
    PG_3bm,
    PG_3bm1,
    PG_3b1m,
    PG_3bm,
    PG_6_m,
    PG_6_m,
    PG_6_m,
    PG_6_mmm,
    PG_6_mmm,
    PG_6_mmm,
    PG_6_mmm,
    PG_6_mmm,
    PG_m3b,
    PG_m3b,
    PG_m3bm,
    PG_m3bm,
    PG_m3bm
  };
#endif /* SGCLIB_C__ */

#ifndef SGCLIB_C__
extern
const char *PG_Names[];
#else
const char *PG_Names[] =
  {
    "Unknown",
    "1",
    "-1",
    "2",
    "m",
    "2/m",
    "222",
    "mm2",
    "mmm",
    "4",
    "-4",
    "4/m",
    "422",
    "4mm",
    "-42m",
    "-4m2",
    "4/mmm",
    "3",
    "-3",
    "321",
    "312",
    "32",
    "3m1",
    "31m",
    "3m",
    "-3m1",
    "-31m",
    "-3m",
    "6",
    "-6",
    "6/m",
    "622",
    "6mm",
    "-6m2",
    "-62m",
    "6/mmm",
    "23",
    "m-3",
    "432",
    "-43m",
    "m-3m"
  };
#endif /* SGCLIB_C__ */


#ifndef SGCLIB_C__
#ifdef SGCOREDEF__
extern const T_LatticeInfo LI_P[];
extern const T_LatticeInfo LI_A[];
extern const T_LatticeInfo LI_B[];
extern const T_LatticeInfo LI_C[];
extern const T_LatticeInfo LI_I[];
extern const T_LatticeInfo LI_R[];
extern const T_LatticeInfo LI_S[];
extern const T_LatticeInfo LI_T[];
extern const T_LatticeInfo LI_F[];
#endif
#else
#define T(i) ((i) * (STBF / 12))
static int         LTr_P[] = { T(0), T(0), T(0)
                             };
const T_LatticeInfo LI_P[] = {{ 'P', 1,  LTr_P
                             }};
static int         LTr_A[] = { T(0), T(0), T(0),
                               T(0), T(6), T(6)
                             };
const T_LatticeInfo LI_A[] = {{ 'A', 2,  LTr_A
                             }};
static int         LTr_B[] = { T(0), T(0), T(0),
                               T(6), T(0), T(6)
                             };
const T_LatticeInfo LI_B[] = {{ 'B', 2,  LTr_B
                             }};
static int         LTr_C[] = { T(0), T(0), T(0),
                               T(6), T(6), T(0)
                             };
const T_LatticeInfo LI_C[] = {{ 'C', 2,  LTr_C
                             }};
static int         LTr_I[] = { T(0), T(0), T(0),
                               T(6), T(6), T(6)
                             };
const T_LatticeInfo LI_I[] = {{ 'I', 2,  LTr_I
                             }};
static int         LTr_R[] = { T(0), T(0), T(0),
                               T(8), T(4), T(4),
                               T(4), T(8), T(8)
                             };
const T_LatticeInfo LI_R[] = {{ 'R', 3,  LTr_R
                             }};
static int         LTr_S[] = { T(0), T(0), T(0),
                               T(4), T(4), T(8),
                               T(8), T(8), T(4)
                             };
const T_LatticeInfo LI_S[] = {{ 'S', 3,  LTr_S
                             }};
static int         LTr_T[] = { T(0), T(0), T(0),
                               T(4), T(8), T(4),
                               T(8), T(4), T(8)
                             };
const T_LatticeInfo LI_T[] = {{ 'T', 3,  LTr_T
                             }};
static int         LTr_F[] = { T(0), T(0), T(0),
                               T(0), T(6), T(6),
                               T(6), T(0), T(6),
                               T(6), T(6), T(0)
                             };
const T_LatticeInfo LI_F[] = {{ 'F', 4,  LTr_F
                             }};
#undef  T
#endif /* SGCLIB_C__ */

/*
                        lattice code
                        R    S    T
         unique axis
                  3z   obv   -   rev
                  3y   rev  obv   -
                  3x    -   rev  obv
 */


#ifndef SGCLIB_C__
#ifdef SGCOREDEF__
extern
const int CCMx_PP[];
extern
const int CCMx_AP[];
extern
const int CCMx_BP[];
extern
const int CCMx_CP[];
extern
const int CCMx_IP[];
extern
const int CCMx_RP_z[];
extern
const int CCMx_SP_y[];
extern
const int CCMx_TP_x[];
extern
const int CCMx_TP_z[];
extern
const int CCMx_RP_y[];
extern
const int CCMx_SP_x[];
extern
const int CCMx_FI_z[];
extern
const int CCMx_FI_y[];
extern
const int CCMx_FI_x[];
extern
const int CCMx_FP[];
#endif
#else
const int CCMx_PP[] =   {  1,  0,  0,  /* Change of Basis Matrices     */
                           0,  1,  0,  /* (coordinate transformations) */
                           0,  0,  1
                        };
const int CCMx_AP[] =   { -1,  0,  0,
                           0, -1,  1,
                           0,  1,  1
                        };
const int CCMx_BP[] =   { -1,  0,  1,
                           0, -1,  0,
                           1,  0,  1
                        };
const int CCMx_CP[] =   {  1,  1,  0,
                           1, -1,  0,
                           0,  0, -1
                        };
const int CCMx_IP[] =   {  0,  1,  1,
                           1,  0,  1,
                           1,  1,  0
                        };
const int CCMx_RP_z[] = {  1,  0,  1,
                          -1,  1,  1,
                           0, -1,  1
                        };
const int CCMx_SP_y[] = {  1,  1, -1,
                          -1,  1,  0,
                           0,  1,  1
                        };
const int CCMx_TP_x[] = {  1,  0, -1,
                           1,  1,  0,
                           1, -1,  1
                        };
const int CCMx_TP_z[] = { -1,  0,  1,
                           1, -1,  1,
                           0,  1,  1
                        };
const int CCMx_RP_y[] = { -1,  1,  1,
                           1,  1,  0,
                           0,  1, -1
                        };
const int CCMx_SP_x[] = {  1,  0,  1,
                           1, -1,  0,
                           1,  1, -1
                        };
const int CCMx_FI_z[] = {  1,  1,  0,
                          -1,  1,  0,
                           0,  0,  1
                        };
const int CCMx_FI_y[] = {  1,  0, -1,
                           0,  1,  0,
                           1,  0,  1
                        };
const int CCMx_FI_x[] = {  1,  0,  0,
                           0,  1,  1,
                           0, -1,  1
                        };
const int CCMx_FP[] =   { -1,  1,  1,
                           1, -1,  1,
                           1,  1, -1
                        };
#endif /* SGCLIB_C__ */


#if defined(SGCLIB_C__) || defined(SGCOREDEF__)
typedef struct
  {
    int  Order;
    int  EigenVector[3];
    int  DirCode;
    int  RMx[9];
  }
  T_TabXtalRotMx;
#endif


#ifndef SGCLIB_C__
#ifdef SGCOREDEF__
extern
const T_TabXtalRotMx TabXtalRotMx[];
#endif
#else
const T_TabXtalRotMx TabXtalRotMx[] =
  {
               /*  #   EigenVector    DirCode */

    {  /* [ 0] */  1, { 0,  0,  0 },  '.',    /* CAUTION:                   */
     { 1,  0,  0,                             /*   Reorganizing this table  */
       0,  1,  0,                             /*   affects RMx_????? below. */
       0,  0,  1 }
    },
    {  /* [ 1] */  2, { 0,  0,  1 },  '=',
     {-1,  0,  0,
       0, -1,  0,
       0,  0,  1 }
    },
    {  /* [ 2] */  2, { 1,  0,  0 },  '=', /* hexagonal */
     { 1, -1,  0,
       0, -1,  0,
       0,  0, -1 }
    },
    {  /* [ 3] */  2, { 0,  1,  0 },  '=', /* hexagonal */
     {-1,  0,  0,
      -1,  1,  0,
       0,  0, -1 }
    },
    {  /* [ 4] */  2, { 1,  1,  0 },  '"',
     { 0,  1,  0,
       1,  0,  0,
       0,  0, -1 }
    },
    {  /* [ 5] */  2, { 1, -1,  0 }, '\'',
     { 0, -1,  0,
      -1,  0,  0,
       0,  0, -1 }
    },
    {  /* [ 6] */  2, { 2,  1,  0 },  '|', /* hexagonal */
     { 1,  0,  0,
       1, -1,  0,
       0,  0, -1 }
    },
    {  /* [ 7] */  2, { 1,  2,  0 }, '\\', /* hexagonal */
     {-1,  1,  0,
       0,  1,  0,
       0,  0, -1 }
    },
    {  /* [ 8] */  3, { 0,  0,  1 },  '=',
     { 0, -1,  0,
       1, -1,  0,
       0,  0,  1 }
    },
    {  /* [ 9] */  3, { 1,  1,  1 },  '*',
     { 0,  0,  1,
       1,  0,  0,
       0,  1,  0 }
    },
    {  /* [10] */  4, { 0,  0,  1 },  '=',
     { 0, -1,  0,
       1,  0,  0,
       0,  0,  1 }
    },
    {  /* [11] */  6, { 0,  0,  1 },  '=',
     { 1, -1,  0,
       1,  0,  0,
       0,  0,  1 }
    },
    {              0, { 0,  0,  0 },  0,
     { 0,  0,  0,
       0,  0,  0,
       0,  0,  0 }
    }
  };
#endif /* SGCLIB_C__ */


#ifndef SGCLIB_C__
#ifdef SGCOREDEF__
extern
const int *RMx_1_000;
extern
const int *RMx_2_001;
extern
const int *RMx_2_110;
extern
const int *RMx_3_001;
extern
const int *RMx_3_111;
extern
const int  RMx_3i111[];
extern
const int *RMx_4_001;
extern
const int  RMx_4i001[];
#endif
#else
const int *RMx_1_000   = TabXtalRotMx[ 0].RMx;
const int *RMx_2_001   = TabXtalRotMx[ 1].RMx;
const int *RMx_2_110   = TabXtalRotMx[ 4].RMx;
const int *RMx_3_001   = TabXtalRotMx[ 8].RMx;
const int *RMx_3_111   = TabXtalRotMx[ 9].RMx;
const int  RMx_3i111[] =
  {
    0,  1,  0,
    0,  0,  1,
    1,  0,  0
  };
const int *RMx_4_001   = TabXtalRotMx[10].RMx;
const int  RMx_4i001[] =
  {
    0,  1,  0,
   -1,  0,  0,
    0,  0,  1
  };
#endif /* SGCLIB_C__ */


#ifndef SGCLIB_C__
#ifdef SGCOREDEF__
extern
const int HallTranslations[];
#endif
#else
#define T(i) ((i) * (STBF / 12))
const int HallTranslations[] =
  {
    'n', T(6), T(6), T(6),
    'a', T(6), T(0), T(0),
    'b', T(0), T(6), T(0),
    'c', T(0), T(0), T(6),
    'd', T(3), T(3), T(3),
    'u', T(3), T(0), T(0),
    'v', T(0), T(3), T(0),
    'w', T(0), T(0), T(3),
      0
  };
#undef  T
#endif


#ifndef SGCLIB_C__
#ifdef SGCOREDEF__
extern
const int VolAPointGroups[];
#endif
#else
const int VolAPointGroups[] =
  {
    PG_Unknown,
    PG_1,
    PG_1b,
    PG_2,
    PG_2,
    PG_2,
    PG_m,
    PG_m,
    PG_m,
    PG_m,
    PG_2_m,
    PG_2_m,
    PG_2_m,
    PG_2_m,
    PG_2_m,
    PG_2_m,
    PG_222,
    PG_222,
    PG_222,
    PG_222,
    PG_222,
    PG_222,
    PG_222,
    PG_222,
    PG_222,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mm2,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_mmm,
    PG_4,
    PG_4,
    PG_4,
    PG_4,
    PG_4,
    PG_4,
    PG_4b,
    PG_4b,
    PG_4_m,
    PG_4_m,
    PG_4_m,
    PG_4_m,
    PG_4_m,
    PG_4_m,
    PG_422,
    PG_422,
    PG_422,
    PG_422,
    PG_422,
    PG_422,
    PG_422,
    PG_422,
    PG_422,
    PG_422,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4mm,
    PG_4b2m,
    PG_4b2m,
    PG_4b2m,
    PG_4b2m,
    PG_4bm2,
    PG_4bm2,
    PG_4bm2,
    PG_4bm2,
    PG_4bm2,
    PG_4bm2,
    PG_4b2m,
    PG_4b2m,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_4_mmm,
    PG_3,
    PG_3,
    PG_3,
    PG_3,
    PG_3b,
    PG_3b,
    PG_312,
    PG_321,
    PG_312,
    PG_321,
    PG_312,
    PG_321,
    PG_32,
    PG_3m1,
    PG_31m,
    PG_3m1,
    PG_31m,
    PG_3m,
    PG_3m,
    PG_3b1m,
    PG_3b1m,
    PG_3bm1,
    PG_3bm1,
    PG_3bm,
    PG_3bm,
    PG_6,
    PG_6,
    PG_6,
    PG_6,
    PG_6,
    PG_6,
    PG_6b,
    PG_6_m,
    PG_6_m,
    PG_622,
    PG_622,
    PG_622,
    PG_622,
    PG_622,
    PG_622,
    PG_6mm,
    PG_6mm,
    PG_6mm,
    PG_6mm,
    PG_6bm2,
    PG_6bm2,
    PG_6b2m,
    PG_6b2m,
    PG_6_mmm,
    PG_6_mmm,
    PG_6_mmm,
    PG_6_mmm,
    PG_23,
    PG_23,
    PG_23,
    PG_23,
    PG_23,
    PG_m3b,
    PG_m3b,
    PG_m3b,
    PG_m3b,
    PG_m3b,
    PG_m3b,
    PG_m3b,
    PG_432,
    PG_432,
    PG_432,
    PG_432,
    PG_432,
    PG_432,
    PG_432,
    PG_432,
    PG_4b3m,
    PG_4b3m,
    PG_4b3m,
    PG_4b3m,
    PG_4b3m,
    PG_4b3m,
    PG_m3bm,
    PG_m3bm,
    PG_m3bm,
    PG_m3bm,
    PG_m3bm,
    PG_m3bm,
    PG_m3bm,
    PG_m3bm,
    PG_m3bm,
    PG_m3bm
  };
#endif /* SGCLIB_C__ */


#ifndef SGCLIB_C__
#ifdef SGCOREDEF__
extern
const char *SchoenfliesSymbols[];
#endif
#else
const char *SchoenfliesSymbols[] =
  {
    NULL,
    "C1^1",
    "Ci^1",
    "C2^1",
    "C2^2",
    "C2^3",
    "Cs^1",
    "Cs^2",
    "Cs^3",
    "Cs^4",
    "C2h^1",
    "C2h^2",
    "C2h^3",
    "C2h^4",
    "C2h^5",
    "C2h^6",
    "D2^1",
    "D2^2",
    "D2^3",
    "D2^4",
    "D2^5",
    "D2^6",
    "D2^7",
    "D2^8",
    "D2^9",
    "C2v^1",
    "C2v^2",
    "C2v^3",
    "C2v^4",
    "C2v^5",
    "C2v^6",
    "C2v^7",
    "C2v^8",
    "C2v^9",
    "C2v^10",
    "C2v^11",
    "C2v^12",
    "C2v^13",
    "C2v^14",
    "C2v^15",
    "C2v^16",
    "C2v^17",
    "C2v^18",
    "C2v^19",
    "C2v^20",
    "C2v^21",
    "C2v^22",
    "D2h^1",
    "D2h^2",
    "D2h^3",
    "D2h^4",
    "D2h^5",
    "D2h^6",
    "D2h^7",
    "D2h^8",
    "D2h^9",
    "D2h^10",
    "D2h^11",
    "D2h^12",
    "D2h^13",
    "D2h^14",
    "D2h^15",
    "D2h^16",
    "D2h^17",
    "D2h^18",
    "D2h^19",
    "D2h^20",
    "D2h^21",
    "D2h^22",
    "D2h^23",
    "D2h^24",
    "D2h^25",
    "D2h^26",
    "D2h^27",
    "D2h^28",
    "C4^1",
    "C4^2",
    "C4^3",
    "C4^4",
    "C4^5",
    "C4^6",
    "S4^1",
    "S4^2",
    "C4h^1",
    "C4h^2",
    "C4h^3",
    "C4h^4",
    "C4h^5",
    "C4h^6",
    "D4^1",
    "D4^2",
    "D4^3",
    "D4^4",
    "D4^5",
    "D4^6",
    "D4^7",
    "D4^8",
    "D4^9",
    "D4^10",
    "C4v^1",
    "C4v^2",
    "C4v^3",
    "C4v^4",
    "C4v^5",
    "C4v^6",
    "C4v^7",
    "C4v^8",
    "C4v^9",
    "C4v^10",
    "C4v^11",
    "C4v^12",
    "D2d^1",
    "D2d^2",
    "D2d^3",
    "D2d^4",
    "D2d^5",
    "D2d^6",
    "D2d^7",
    "D2d^8",
    "D2d^9",
    "D2d^10",
    "D2d^11",
    "D2d^12",
    "D4h^1",
    "D4h^2",
    "D4h^3",
    "D4h^4",
    "D4h^5",
    "D4h^6",
    "D4h^7",
    "D4h^8",
    "D4h^9",
    "D4h^10",
    "D4h^11",
    "D4h^12",
    "D4h^13",
    "D4h^14",
    "D4h^15",
    "D4h^16",
    "D4h^17",
    "D4h^18",
    "D4h^19",
    "D4h^20",
    "C3^1",
    "C3^2",
    "C3^3",
    "C3^4",
    "C3i^1",
    "C3i^2",
    "D3^1",
    "D3^2",
    "D3^3",
    "D3^4",
    "D3^5",
    "D3^6",
    "D3^7",
    "C3v^1",
    "C3v^2",
    "C3v^3",
    "C3v^4",
    "C3v^5",
    "C3v^6",
    "D3d^1",
    "D3d^2",
    "D3d^3",
    "D3d^4",
    "D3d^5",
    "D3d^6",
    "C6^1",
    "C6^2",
    "C6^3",
    "C6^4",
    "C6^5",
    "C6^6",
    "C3h^1",
    "C6h^1",
    "C6h^2",
    "D6^1",
    "D6^2",
    "D6^3",
    "D6^4",
    "D6^5",
    "D6^6",
    "C6v^1",
    "C6v^2",
    "C6v^3",
    "C6v^4",
    "D3h^1",
    "D3h^2",
    "D3h^3",
    "D3h^4",
    "D6h^1",
    "D6h^2",
    "D6h^3",
    "D6h^4",
    "T^1",
    "T^2",
    "T^3",
    "T^4",
    "T^5",
    "Th^1",
    "Th^2",
    "Th^3",
    "Th^4",
    "Th^5",
    "Th^6",
    "Th^7",
    "O^1",
    "O^2",
    "O^3",
    "O^4",
    "O^5",
    "O^6",
    "O^7",
    "O^8",
    "Td^1",
    "Td^2",
    "Td^3",
    "Td^4",
    "Td^5",
    "Td^6",
    "Oh^1",
    "Oh^2",
    "Oh^3",
    "Oh^4",
    "Oh^5",
    "Oh^6",
    "Oh^7",
    "Oh^8",
    "Oh^9",
    "Oh^10"
  };
#endif /* SGCLIB_C__ */


#ifndef SGCLIB_C__
#ifdef SGCOREDEF__
extern
const T_TabSgName TabSgName[];
#endif
#else
const T_TabSgName TabSgName[] =
  {
    { " P 1",                1, "",      "P_1" },
    { "-P 1",                2, "",      "P_-1" },
    { " P 2y",               3, "b",     "P_2 = P_1_2_1" },
    { " P 2",                3, "c",     "P_2 = P_1_1_2" },
    { " P 2x",               3, "a",     "P_2 = P_2_1_1" },
    { " P 2yb",              4, "b",     "P_21 = P_1_21_1" },
    { " P 2c",               4, "c",     "P_21 = P_1_1_21" },
    { " P 2xa",              4, "a",     "P_21 = P_21_1_1" },
    { " C 2y",               5, "b1",    "C_2 = C_1_2_1" },
    { " A 2y",               5, "b2",    "C_2 = A_1_2_1" },
    { " I 2y",               5, "b3",    "C_2 = I_1_2_1" },
    { " A 2",                5, "c1",    "C_2 = A_1_1_2" },
    { " B 2",                5, "c2",    "C_2 = B_1_1_2 = B_2" },
    { " I 2",                5, "c3",    "C_2 = I_1_1_2" },
    { " B 2x",               5, "a1",    "C_2 = B_2_1_1" },
    { " C 2x",               5, "a2",    "C_2 = C_2_1_1" },
    { " I 2x",               5, "a3",    "C_2 = I_2_1_1" },
    { " P -2y",              6, "b",     "P_m = P_1_m_1" },
    { " P -2",               6, "c",     "P_m = P_1_1_m" },
    { " P -2x",              6, "a",     "P_m = P_m_1_1" },
    { " P -2yc",             7, "b1",    "P_c = P_1_c_1" },
    { " P -2yac",            7, "b2",    "P_c = P_1_n_1" },
    { " P -2ya",             7, "b3",    "P_c = P_1_a_1" },
    { " P -2a",              7, "c1",    "P_c = P_1_1_a" },
    { " P -2ab",             7, "c2",    "P_c = P_1_1_n" },
    { " P -2b",              7, "c3",    "P_c = P_1_1_b = P_b" },
    { " P -2xb",             7, "a1",    "P_c = P_b_1_1" },
    { " P -2xbc",            7, "a2",    "P_c = P_n_1_1" },
    { " P -2xc",             7, "a3",    "P_c = P_c_1_1" },
    { " C -2y",              8, "b1",    "C_m = C_1_m_1" },
    { " A -2y",              8, "b2",    "C_m = A_1_m_1" },
    { " I -2y",              8, "b3",    "C_m = I_1_m_1" },
    { " A -2",               8, "c1",    "C_m = A_1_1_m" },
    { " B -2",               8, "c2",    "C_m = B_1_1_m = B_m" },
    { " I -2",               8, "c3",    "C_m = I_1_1_m" },
    { " B -2x",              8, "a1",    "C_m = B_m_1_1" },
    { " C -2x",              8, "a2",    "C_m = C_m_1_1" },
    { " I -2x",              8, "a3",    "C_m = I_m_1_1" },
    { " C -2yc",             9, "b1",    "C_c = C_1_c_1" },
    { " A -2yac",            9, "b2",    "C_c = A_1_n_1" },
    { " I -2ya",             9, "b3",    "C_c = I_1_a_1" },
    { " A -2ya",             9, "-b1",   "C_c = A_1_a_1" },
    { " C -2ybc",            9, "-b2",   "C_c = C_1_n_1" },
    { " I -2yc",             9, "-b3",   "C_c = I_1_c_1" },
    { " A -2a",              9, "c1",    "C_c = A_1_1_a" },
    { " B -2bc",             9, "c2",    "C_c = B_1_1_n" },
    { " I -2b",              9, "c3",    "C_c = I_1_1_b" },
    { " B -2b",              9, "-c1",   "C_c = B_1_1_b = B_b" },
    { " A -2ac",             9, "-c2",   "C_c = A_1_1_n" },
    { " I -2a",              9, "-c3",   "C_c = I_1_1_a" },
    { " B -2xb",             9, "a1",    "C_c = B_b_1_1" },
    { " C -2xbc",            9, "a2",    "C_c = C_n_1_1" },
    { " I -2xc",             9, "a3",    "C_c = I_c_1_1" },
    { " C -2xc",             9, "-a1",   "C_c = C_c_1_1" },
    { " B -2xbc",            9, "-a2",   "C_c = B_n_1_1" },
    { " I -2xb",             9, "-a3",   "C_c = I_b_1_1" },
    { "-P 2y",              10, "b",     "P_2/m = P_1_2/m_1" },
    { "-P 2",               10, "c",     "P_2/m = P_1_1_2/m" },
    { "-P 2x",              10, "a",     "P_2/m = P_2/m_1_1" },
    { "-P 2yb",             11, "b",     "P_21/m = P_1_21/m_1" },
    { "-P 2c",              11, "c",     "P_21/m = P_1_1_21/m" },
    { "-P 2xa",             11, "a",     "P_21/m = P_21/m_1_1" },
    { "-C 2y",              12, "b1",    "C_2/m = C_1_2/m_1" },
    { "-A 2y",              12, "b2",    "C_2/m = A_1_2/m_1" },
    { "-I 2y",              12, "b3",    "C_2/m = I_1_2/m_1" },
    { "-A 2",               12, "c1",    "C_2/m = A_1_1_2/m" },
    { "-B 2",               12, "c2",    "C_2/m = B_1_1_2/m = B_2/m" },
    { "-I 2",               12, "c3",    "C_2/m = I_1_1_2/m" },
    { "-B 2x",              12, "a1",    "C_2/m = B_2/m_1_1" },
    { "-C 2x",              12, "a2",    "C_2/m = C_2/m_1_1" },
    { "-I 2x",              12, "a3",    "C_2/m = I_2/m_1_1" },
    { "-P 2yc",             13, "b1",    "P_2/c = P_1_2/c_1" },
    { "-P 2yac",            13, "b2",    "P_2/c = P_1_2/n_1" },
    { "-P 2ya",             13, "b3",    "P_2/c = P_1_2/a_1" },
    { "-P 2a",              13, "c1",    "P_2/c = P_1_1_2/a" },
    { "-P 2ab",             13, "c2",    "P_2/c = P_1_1_2/n" },
    { "-P 2b",              13, "c3",    "P_2/c = P_1_1_2/b = P_2/b" },
    { "-P 2xb",             13, "a1",    "P_2/c = P_2/b_1_1" },
    { "-P 2xbc",            13, "a2",    "P_2/c = P_2/n_1_1" },
    { "-P 2xc",             13, "a3",    "P_2/c = P_2/c_1_1" },
    { "-P 2ybc",            14, "b1",    "P_21/c = P_1_21/c_1" },
    { "-P 2yn",             14, "b2",    "P_21/c = P_1_21/n_1" },
    { "-P 2yab",            14, "b3",    "P_21/c = P_1_21/a_1" },
    { "-P 2ac",             14, "c1",    "P_21/c = P_1_1_21/a" },
    { "-P 2n",              14, "c2",    "P_21/c = P_1_1_21/n" },
    { "-P 2bc",             14, "c3",    "P_21/c = P_1_1_21/b = P_21/b" },
    { "-P 2xab",            14, "a1",    "P_21/c = P_21/b_1_1" },
    { "-P 2xn",             14, "a2",    "P_21/c = P_21/n_1_1" },
    { "-P 2xac",            14, "a3",    "P_21/c = P_21/c_1_1" },
    { "-C 2yc",             15, "b1",    "C_2/c = C_1_2/c_1" },
    { "-A 2yac",            15, "b2",    "C_2/c = A_1_2/n_1" },
    { "-I 2ya",             15, "b3",    "C_2/c = I_1_2/a_1" },
    { "-A 2ya",             15, "-b1",   "C_2/c = A_1_2/a_1" },
    { "-C 2ybc",            15, "-b2",   "C_2/c = C_1_2/n_1" },
    { "-I 2yc",             15, "-b3",   "C_2/c = I_1_2/c_1" },
    { "-A 2a",              15, "c1",    "C_2/c = A_1_1_2/a" },
    { "-B 2bc",             15, "c2",    "C_2/c = B_1_1_2/n" },
    { "-I 2b",              15, "c3",    "C_2/c = I_1_1_2/b" },
    { "-B 2b",              15, "-c1",   "C_2/c = B_1_1_2/b = B_2/b" },
    { "-A 2ac",             15, "-c2",   "C_2/c = A_1_1_2/n" },
    { "-I 2a",              15, "-c3",   "C_2/c = I_1_1_2/a" },
    { "-B 2xb",             15, "a1",    "C_2/c = B_2/b_1_1" },
    { "-C 2xbc",            15, "a2",    "C_2/c = C_2/n_1_1" },
    { "-I 2xc",             15, "a3",    "C_2/c = I_2/c_1_1" },
    { "-C 2xc",             15, "-a1",   "C_2/c = C_2/c_1_1" },
    { "-B 2xbc",            15, "-a2",   "C_2/c = B_2/n_1_1" },
    { "-I 2xb",             15, "-a3",   "C_2/c = I_2/b_1_1" },
    { " P 2 2",             16, "",      "P_2_2_2" },
    { " P 2c 2",            17, "",      "P_2_2_21" },
    { " P 2a 2a",           17, "cab",   "P_21_2_2" },
    { " P 2 2b",            17, "bca",   "P_2_21_2" },
    { " P 2 2ab",           18, "",      "P_21_21_2" },
    { " P 2bc 2",           18, "cab",   "P_2_21_21" },
    { " P 2ac 2ac",         18, "bca",   "P_21_2_21" },
    { " P 2ac 2ab",         19, "",      "P_21_21_21" },
    { " C 2c 2",            20, "",      "C_2_2_21" },
    { " A 2a 2a",           20, "cab",   "A_21_2_2" },
    { " B 2 2b",            20, "bca",   "B_2_21_2" },
    { " C 2 2",             21, "",      "C_2_2_2" },
    { " A 2 2",             21, "cab",   "A_2_2_2" },
    { " B 2 2",             21, "bca",   "B_2_2_2" },
    { " F 2 2",             22, "",      "F_2_2_2" },
    { " I 2 2",             23, "",      "I_2_2_2" },
    { " I 2b 2c",           24, "",      "I_21_21_21" },
    { " P 2 -2",            25, "",      "P_m_m_2" },
    { " P -2 2",            25, "cab",   "P_2_m_m" },
    { " P -2 -2",           25, "bca",   "P_m_2_m" },
    { " P 2c -2",           26, "",      "P_m_c_21" },
    { " P 2c -2c",          26, "ba-c",  "P_c_m_21" },
    { " P -2a 2a",          26, "cab",   "P_21_m_a" },
    { " P -2 2a",           26, "-cba",  "P_21_a_m" },
    { " P -2 -2b",          26, "bca",   "P_b_21_m" },
    { " P -2b -2",          26, "a-cb",  "P_m_21_b" },
    { " P 2 -2c",           27, "",      "P_c_c_2" },
    { " P -2a 2",           27, "cab",   "P_2_a_a" },
    { " P -2b -2b",         27, "bca",   "P_b_2_b" },
    { " P 2 -2a",           28, "",      "P_m_a_2" },
    { " P 2 -2b",           28, "ba-c",  "P_b_m_2" },
    { " P -2b 2",           28, "cab",   "P_2_m_b" },
    { " P -2c 2",           28, "-cba",  "P_2_c_m" },
    { " P -2c -2c",         28, "bca",   "P_c_2_m" },
    { " P -2a -2a",         28, "a-cb",  "P_m_2_a" },
    { " P 2c -2ac",         29, "",      "P_c_a_21" },
    { " P 2c -2b",          29, "ba-c",  "P_b_c_21" },
    { " P -2b 2a",          29, "cab",   "P_21_a_b" },
    { " P -2ac 2a",         29, "-cba",  "P_21_c_a" },
    { " P -2bc -2c",        29, "bca",   "P_c_21_b" },
    { " P -2a -2ab",        29, "a-cb",  "P_b_21_a" },
    { " P 2 -2bc",          30, "",      "P_n_c_2" },
    { " P 2 -2ac",          30, "ba-c",  "P_c_n_2" },
    { " P -2ac 2",          30, "cab",   "P_2_n_a" },
    { " P -2ab 2",          30, "-cba",  "P_2_a_n" },
    { " P -2ab -2ab",       30, "bca",   "P_b_2_n" },
    { " P -2bc -2bc",       30, "a-cb",  "P_n_2_b" },
    { " P 2ac -2",          31, "",      "P_m_n_21" },
    { " P 2bc -2bc",        31, "ba-c",  "P_n_m_21" },
    { " P -2ab 2ab",        31, "cab",   "P_21_m_n" },
    { " P -2 2ac",          31, "-cba",  "P_21_n_m" },
    { " P -2 -2bc",         31, "bca",   "P_n_21_m" },
    { " P -2ab -2",         31, "a-cb",  "P_m_21_n" },
    { " P 2 -2ab",          32, "",      "P_b_a_2" },
    { " P -2bc 2",          32, "cab",   "P_2_c_b" },
    { " P -2ac -2ac",       32, "bca",   "P_c_2_a" },
    { " P 2c -2n",          33, "",      "P_n_a_21" },
    { " P 2c -2ab",         33, "ba-c",  "P_b_n_21" },
    { " P -2bc 2a",         33, "cab",   "P_21_n_b" },
    { " P -2n 2a",          33, "-cba",  "P_21_c_n" },
    { " P -2n -2ac",        33, "bca",   "P_c_21_n" },
    { " P -2ac -2n",        33, "a-cb",  "P_n_21_a" },
    { " P 2 -2n",           34, "",      "P_n_n_2" },
    { " P -2n 2",           34, "cab",   "P_2_n_n" },
    { " P -2n -2n",         34, "bca",   "P_n_2_n" },
    { " C 2 -2",            35, "",      "C_m_m_2" },
    { " A -2 2",            35, "cab",   "A_2_m_m" },
    { " B -2 -2",           35, "bca",   "B_m_2_m" },
    { " C 2c -2",           36, "",      "C_m_c_21" },
    { " C 2c -2c",          36, "ba-c",  "C_c_m_21" },
    { " A -2a 2a",          36, "cab",   "A_21_m_a" },
    { " A -2 2a",           36, "-cba",  "A_21_a_m" },
    { " B -2 -2b",          36, "bca",   "B_b_21_m" },
    { " B -2b -2",          36, "a-cb",  "B_m_21_b" },
    { " C 2 -2c",           37, "",      "C_c_c_2" },
    { " A -2a 2",           37, "cab",   "A_2_a_a" },
    { " B -2b -2b",         37, "bca",   "B_b_2_b" },
    { " A 2 -2",            38, "",      "A_m_m_2" },
    { " B 2 -2",            38, "ba-c",  "B_m_m_2" },
    { " B -2 2",            38, "cab",   "B_2_m_m" },
    { " C -2 2",            38, "-cba",  "C_2_m_m" },
    { " C -2 -2",           38, "bca",   "C_m_2_m" },
    { " A -2 -2",           38, "a-cb",  "A_m_2_m" },
    { " A 2 -2c",           39, "",      "A_b_m_2" },
    { " B 2 -2c",           39, "ba-c",  "B_m_a_2" },
    { " B -2c 2",           39, "cab",   "B_2_c_m" },
    { " C -2b 2",           39, "-cba",  "C_2_m_b" },
    { " C -2b -2b",         39, "bca",   "C_m_2_a" },
    { " A -2c -2c",         39, "a-cb",  "A_c_2_m" },
    { " A 2 -2a",           40, "",      "A_m_a_2" },
    { " B 2 -2b",           40, "ba-c",  "B_b_m_2" },
    { " B -2b 2",           40, "cab",   "B_2_m_b" },
    { " C -2c 2",           40, "-cba",  "C_2_c_m" },
    { " C -2c -2c",         40, "bca",   "C_c_2_m" },
    { " A -2a -2a",         40, "a-cb",  "A_m_2_a" },
    { " A 2 -2ac",          41, "",      "A_b_a_2" },
    { " B 2 -2bc",          41, "ba-c",  "B_b_a_2" },
    { " B -2bc 2",          41, "cab",   "B_2_c_b" },
    { " C -2bc 2",          41, "-cba",  "C_2_c_b" },
    { " C -2bc -2bc",       41, "bca",   "C_c_2_a" },
    { " A -2ac -2ac",       41, "a-cb",  "A_c_2_a" },
    { " F 2 -2",            42, "",      "F_m_m_2" },
    { " F -2 2",            42, "cab",   "F_2_m_m" },
    { " F -2 -2",           42, "bca",   "F_m_2_m" },
    { " F 2 -2d",           43, "",      "F_d_d_2" },
    { " F -2d 2",           43, "cab",   "F_2_d_d" },
    { " F -2d -2d",         43, "bca",   "F_d_2_d" },
    { " I 2 -2",            44, "",      "I_m_m_2" },
    { " I -2 2",            44, "cab",   "I_2_m_m" },
    { " I -2 -2",           44, "bca",   "I_m_2_m" },
    { " I 2 -2c",           45, "",      "I_b_a_2" },
    { " I -2a 2",           45, "cab",   "I_2_c_b" },
    { " I -2b -2b",         45, "bca",   "I_c_2_a" },
    { " I 2 -2a",           46, "",      "I_m_a_2" },
    { " I 2 -2b",           46, "ba-c",  "I_b_m_2" },
    { " I -2b 2",           46, "cab",   "I_2_m_b" },
    { " I -2c 2",           46, "-cba",  "I_2_c_m" },
    { " I -2c -2c",         46, "bca",   "I_c_2_m" },
    { " I -2a -2a",         46, "a-cb",  "I_m_2_a" },
    { "-P 2 2",             47, "",      "P_m_m_m" },
    { " P 2 2 -1n",         48, "1",     "P_n_n_n" },
    { "-P 2ab 2bc",         48, "2",     "P_n_n_n" },
    { "-P 2 2c",            49, "",      "P_c_c_m" },
    { "-P 2a 2",            49, "cab",   "P_m_a_a" },
    { "-P 2b 2b",           49, "bca",   "P_b_m_b" },
    { " P 2 2 -1ab",        50, "1",     "P_b_a_n" },
    { "-P 2ab 2b",          50, "2",     "P_b_a_n" },
    { " P 2 2 -1bc",        50, "1cab",  "P_n_c_b" },
    { "-P 2b 2bc",          50, "2cab",  "P_n_c_b" },
    { " P 2 2 -1ac",        50, "1bca",  "P_c_n_a" },
    { "-P 2a 2c",           50, "2bca",  "P_c_n_a" },
    { "-P 2a 2a",           51, "",      "P_m_m_a" },
    { "-P 2b 2",            51, "ba-c",  "P_m_m_b" },
    { "-P 2 2b",            51, "cab",   "P_b_m_m" },
    { "-P 2c 2c",           51, "-cba",  "P_c_m_m" },
    { "-P 2c 2",            51, "bca",   "P_m_c_m" },
    { "-P 2 2a",            51, "a-cb",  "P_m_a_m" },
    { "-P 2a 2bc",          52, "",      "P_n_n_a" },
    { "-P 2b 2n",           52, "ba-c",  "P_n_n_b" },
    { "-P 2n 2b",           52, "cab",   "P_b_n_n" },
    { "-P 2ab 2c",          52, "-cba",  "P_c_n_n" },
    { "-P 2ab 2n",          52, "bca",   "P_n_c_n" },
    { "-P 2n 2bc",          52, "a-cb",  "P_n_a_n" },
    { "-P 2ac 2",           53, "",      "P_m_n_a" },
    { "-P 2bc 2bc",         53, "ba-c",  "P_n_m_b" },
    { "-P 2ab 2ab",         53, "cab",   "P_b_m_n" },
    { "-P 2 2ac",           53, "-cba",  "P_c_n_m" },
    { "-P 2 2bc",           53, "bca",   "P_n_c_m" },
    { "-P 2ab 2",           53, "a-cb",  "P_m_a_n" },
    { "-P 2a 2ac",          54, "",      "P_c_c_a" },
    { "-P 2b 2c",           54, "ba-c",  "P_c_c_b" },
    { "-P 2a 2b",           54, "cab",   "P_b_a_a" },
    { "-P 2ac 2c",          54, "-cba",  "P_c_a_a" },
    { "-P 2bc 2b",          54, "bca",   "P_b_c_b" },
    { "-P 2b 2ab",          54, "a-cb",  "P_b_a_b" },
    { "-P 2 2ab",           55, "",      "P_b_a_m" },
    { "-P 2bc 2",           55, "cab",   "P_m_c_b" },
    { "-P 2ac 2ac",         55, "bca",   "P_c_m_a" },
    { "-P 2ab 2ac",         56, "",      "P_c_c_n" },
    { "-P 2ac 2bc",         56, "cab",   "P_n_a_a" },
    { "-P 2bc 2ab",         56, "bca",   "P_b_n_b" },
    { "-P 2c 2b",           57, "",      "P_b_c_m" },
    { "-P 2c 2ac",          57, "ba-c",  "P_c_a_m" },
    { "-P 2ac 2a",          57, "cab",   "P_m_c_a" },
    { "-P 2b 2a",           57, "-cba",  "P_m_a_b" },
    { "-P 2a 2ab",          57, "bca",   "P_b_m_a" },
    { "-P 2bc 2c",          57, "a-cb",  "P_c_m_b" },
    { "-P 2 2n",            58, "",      "P_n_n_m" },
    { "-P 2n 2",            58, "cab",   "P_m_n_n" },
    { "-P 2n 2n",           58, "bca",   "P_n_m_n" },
    { " P 2 2ab -1ab",      59, "1",     "P_m_m_n" },
    { "-P 2ab 2a",          59, "2",     "P_m_m_n" },
    { " P 2bc 2 -1bc",      59, "1cab",  "P_n_m_m" },
    { "-P 2c 2bc",          59, "2cab",  "P_n_m_m" },
    { " P 2ac 2ac -1ac",    59, "1bca",  "P_m_n_m" },
    { "-P 2c 2a",           59, "2bca",  "P_m_n_m" },
    { "-P 2n 2ab",          60, "",      "P_b_c_n" },
    { "-P 2n 2c",           60, "ba-c",  "P_c_a_n" },
    { "-P 2a 2n",           60, "cab",   "P_n_c_a" },
    { "-P 2bc 2n",          60, "-cba",  "P_n_a_b" },
    { "-P 2ac 2b",          60, "bca",   "P_b_n_a" },
    { "-P 2b 2ac",          60, "a-cb",  "P_c_n_b" },
    { "-P 2ac 2ab",         61, "",      "P_b_c_a" },
    { "-P 2bc 2ac",         61, "ba-c",  "P_c_a_b" },
    { "-P 2ac 2n",          62, "",      "P_n_m_a" },
    { "-P 2bc 2a",          62, "ba-c",  "P_m_n_b" },
    { "-P 2c 2ab",          62, "cab",   "P_b_n_m" },
    { "-P 2n 2ac",          62, "-cba",  "P_c_m_n" },
    { "-P 2n 2a",           62, "bca",   "P_m_c_n" },
    { "-P 2c 2n",           62, "a-cb",  "P_n_a_m" },
    { "-C 2c 2",            63, "",      "C_m_c_m" },
    { "-C 2c 2c",           63, "ba-c",  "C_c_m_m" },
    { "-A 2a 2a",           63, "cab",   "A_m_m_a" },
    { "-A 2 2a",            63, "-cba",  "A_m_a_m" },
    { "-B 2 2b",            63, "bca",   "B_b_m_m" },
    { "-B 2b 2",            63, "a-cb",  "B_m_m_b" },
    { "-C 2bc 2",           64, "",      "C_m_c_a" },
    { "-C 2bc 2bc",         64, "ba-c",  "C_c_m_b" },
    { "-A 2ac 2ac",         64, "cab",   "A_b_m_a" },
    { "-A 2 2ac",           64, "-cba",  "A_c_a_m" },
    { "-B 2 2bc",           64, "bca",   "B_b_c_m" },
    { "-B 2bc 2",           64, "a-cb",  "B_m_a_b" },
    { "-C 2 2",             65, "",      "C_m_m_m" },
    { "-A 2 2",             65, "cab",   "A_m_m_m" },
    { "-B 2 2",             65, "bca",   "B_m_m_m" },
    { "-C 2 2c",            66, "",      "C_c_c_m" },
    { "-A 2a 2",            66, "cab",   "A_m_a_a" },
    { "-B 2b 2b",           66, "bca",   "B_b_m_b" },
    { "-C 2b 2",            67, "",      "C_m_m_a" },
    { "-C 2b 2b",           67, "ba-c",  "C_m_m_b" },
    { "-A 2c 2c",           67, "cab",   "A_b_m_m" },
    { "-A 2 2c",            67, "-cba",  "A_c_m_m" },
    { "-B 2 2c",            67, "bca",   "B_m_c_m" },
    { "-B 2c 2",            67, "a-cb",  "B_m_a_m" },
    { " C 2 2 -1bc",        68, "1",     "C_c_c_a" },
    { "-C 2b 2bc",          68, "2",     "C_c_c_a" },
    { " C 2 2 -1bc",        68, "1ba-c", "C_c_c_b" },
    { "-C 2b 2c",           68, "2ba-c", "C_c_c_b" },
    { " A 2 2 -1ac",        68, "1cab",  "A_b_a_a" },
    { "-A 2a 2c",           68, "2cab",  "A_b_a_a" },
    { " A 2 2 -1ac",        68, "1-cba", "A_c_a_a" },
    { "-A 2ac 2c",          68, "2-cba", "A_c_a_a" },
    { " B 2 2 -1bc",        68, "1bca",  "B_b_c_b" },
    { "-B 2bc 2b",          68, "2bca",  "B_b_c_b" },
    { " B 2 2 -1bc",        68, "1a-cb", "B_b_a_b" },
    { "-B 2b 2bc",          68, "2a-cb", "B_b_a_b" },
    { "-F 2 2",             69, "",      "F_m_m_m" },
    { " F 2 2 -1d",         70, "1",     "F_d_d_d" },
    { "-F 2uv 2vw",         70, "2",     "F_d_d_d" },
    { "-I 2 2",             71, "",      "I_m_m_m" },
    { "-I 2 2c",            72, "",      "I_b_a_m" },
    { "-I 2a 2",            72, "cab",   "I_m_c_b" },
    { "-I 2b 2b",           72, "bca",   "I_c_m_a" },
    { "-I 2b 2c",           73, "",      "I_b_c_a" },
    { "-I 2a 2b",           73, "ba-c",  "I_c_a_b" },
    { "-I 2b 2",            74, "",      "I_m_m_a" },
    { "-I 2a 2a",           74, "ba-c",  "I_m_m_b" },
    { "-I 2c 2c",           74, "cab",   "I_b_m_m" },
    { "-I 2 2b",            74, "-cba",  "I_c_m_m" },
    { "-I 2 2a",            74, "bca",   "I_m_c_m" },
    { "-I 2c 2",            74, "a-cb",  "I_m_a_m" },
    { " P 4",               75, "",      "P_4" },
    { " P 4w",              76, "",      "P_41" },
    { " P 4c",              77, "",      "P_42" },
    { " P 4cw",             78, "",      "P_43" },
    { " I 4",               79, "",      "I_4" },
    { " I 4bw",             80, "",      "I_41" },
    { " P -4",              81, "",      "P_-4" },
    { " I -4",              82, "",      "I_-4" },
    { "-P 4",               83, "",      "P_4/m" },
    { "-P 4c",              84, "",      "P_42/m" },
    { " P 4ab -1ab",        85, "1",     "P_4/n" },
    { "-P 4a",              85, "2",     "P_4/n" },
    { " P 4n -1n",          86, "1",     "P_42/n" },
    { "-P 4bc",             86, "2",     "P_42/n" },
    { "-I 4",               87, "",      "I_4/m" },
    { " I 4bw -1bw",        88, "1",     "I_41/a" },
    { "-I 4ad",             88, "2",     "I_41/a" },
    { " P 4 2",             89, "",      "P_4_2_2" },
    { " P 4ab 2ab",         90, "",      "P_4_21_2" },   /*wrong P_42_1_2, correct P_4_21_2*/
    { " P 4w 2c",           91, "",      "P_41_2_2" },
    { " P 4abw 2nw",        92, "",      "P_41_21_2" },
    { " P 4c 2",            93, "",      "P_42_2_2" },
    { " P 4n 2n",           94, "",      "P_42_21_2" },
    { " P 4cw 2c",          95, "",      "P_43_2_2" },
    { " P 4nw 2abw",        96, "",      "P_43_21_2" },
    { " I 4 2",             97, "",      "I_4_2_2" },
    { " I 4bw 2bw",         98, "",      "I_41_2_2" },
    { " P 4 -2",            99, "",      "P_4_m_m" },
    { " P 4 -2ab",         100, "",      "P_4_b_m" },
    { " P 4c -2c",         101, "",      "P_42_c_m" },
    { " P 4n -2n",         102, "",      "P_42_n_m" },
    { " P 4 -2c",          103, "",      "P_4_c_c" },
    { " P 4 -2n",          104, "",      "P_4_n_c" },
    { " P 4c -2",          105, "",      "P_42_m_c" },
    { " P 4c -2ab",        106, "",      "P_42_b_c" },
    { " I 4 -2",           107, "",      "I_4_m_m" },
    { " I 4 -2c",          108, "",      "I_4_c_m" },
    { " I 4bw -2",         109, "",      "I_41_m_d" },
    { " I 4bw -2c",        110, "",      "I_41_c_d" },
    { " P -4 2",           111, "",      "P_-4_2_m" },
    { " P -4 2c",          112, "",      "P_-4_2_c" },
    { " P -4 2ab",         113, "",      "P_-4_21_m" },
    { " P -4 2n",          114, "",      "P_-4_21_c" },
    { " P -4 -2",          115, "",      "P_-4_m_2" },
    { " P -4 -2c",         116, "",      "P_-4_c_2" },
    { " P -4 -2ab",        117, "",      "P_-4_b_2" },
    { " P -4 -2n",         118, "",      "P_-4_n_2" },
    { " I -4 -2",          119, "",      "I_-4_m_2" },
    { " I -4 -2c",         120, "",      "I_-4_c_2" },
    { " I -4 2",           121, "",      "I_-4_2_m" },
    { " I -4 2bw",         122, "",      "I_-4_2_d" },
    { "-P 4 2",            123, "",      "P_4/m_m_m" },
    { "-P 4 2c",           124, "",      "P_4/m_c_c" },
    { " P 4 2 -1ab",       125, "1",     "P_4/n_b_m" },
    { "-P 4a 2b",          125, "2",     "P_4/n_b_m" },
    { " P 4 2 -1n",        126, "1",     "P_4/n_n_c" },
    { "-P 4a 2bc",         126, "2",     "P_4/n_n_c" },
    { "-P 4 2ab",          127, "",      "P_4/m_b_m" },
    { "-P 4 2n",           128, "",      "P_4/m_n_c" },
    { " P 4ab 2ab -1ab",   129, "1",     "P_4/n_m_m" },
    { "-P 4a 2a",          129, "2",     "P_4/n_m_m" },
    { " P 4ab 2n -1ab",    130, "1",     "P_4/n_c_c" },
    { "-P 4a 2ac",         130, "2",     "P_4/n_c_c" },
    { "-P 4c 2",           131, "",      "P_42/m_m_c" },
    { "-P 4c 2c",          132, "",      "P_42/m_c_m" },
    { " P 4n 2c -1n",      133, "1",     "P_42/n_b_c" },
    { "-P 4ac 2b",         133, "2",     "P_42/n_b_c" },
    { " P 4n 2 -1n",       134, "1",     "P_42/n_n_m" },
    { "-P 4ac 2bc",        134, "2",     "P_42/n_n_m" },
    { "-P 4c 2ab",         135, "",      "P_42/m_b_c" },
    { "-P 4n 2n",          136, "",      "P_42/m_n_m" },
    { " P 4n 2n -1n",      137, "1",     "P_42/n_m_c" },
    { "-P 4ac 2a",         137, "2",     "P_42/n_m_c" },
    { " P 4n 2ab -1n",     138, "1",     "P_42/n_c_m" },
    { "-P 4ac 2ac",        138, "2",     "P_42/n_c_m" },
    { "-I 4 2",            139, "",      "I_4/m_m_m" },
    { "-I 4 2c",           140, "",      "I_4/m_c_m" },
    { " I 4bw 2bw -1bw",   141, "1",     "I_41/a_m_d" },
    { "-I 4bd 2",          141, "2",     "I_41/a_m_d" },
    { " I 4bw 2aw -1bw",   142, "1",     "I_41/a_c_d" },
    { "-I 4bd 2c",         142, "2",     "I_41/a_c_d" },
    { " P 3",              143, "",      "P_3" },
    { " P 31",             144, "",      "P_31" },
    { " P 32",             145, "",      "P_32" },
    { " R 3",              146, "H",     "R_3" },
    { " P 3*",             146, "R",     "R_3" },
    { "-P 3",              147, "",      "P_-3" },
    { "-R 3",              148, "H",     "R_-3" },
    { "-P 3*",             148, "R",     "R_-3" },
    { " P 3 2",            149, "",      "P_3_1_2" },
    { " P 3 2\"",          150, "",      "P_3_2_1" },
    { " P 31 2c (0 0 1)",  151, "",      "P_31_1_2" },
    { " P 31 2\"",         152, "",      "P_31_2_1" },
    { " P 32 2c (0 0 -1)", 153, "",      "P_32_1_2" },
    { " P 32 2\"",         154, "",      "P_32_2_1" },
    { " R 3 2\"",          155, "H",     "R_3_2" },      /*wrong R_32, correct R_3_2*/
    { " P 3* 2",           155, "R",     "R_3_2" },      /*wrong R_32, correct R_3_2*/
    { " P 3 -2\"",         156, "",      "P_3_m_1" },
    { " P 3 -2",           157, "",      "P_3_1_m" },
    { " P 3 -2\"c",        158, "",      "P_3_c_1" },
    { " P 3 -2c",          159, "",      "P_3_1_c" },
    { " R 3 -2\"",         160, "H",     "R_3_m" },
    { " P 3* -2",          160, "R",     "R_3_m" },
    { " R 3 -2\"c",        161, "H",     "R_3_c" },
    { " P 3* -2n",         161, "R",     "R_3_c" },
    { "-P 3 2",            162, "",      "P_-3_1_m" },
    { "-P 3 2c",           163, "",      "P_-3_1_c" },
    { "-P 3 2\"",          164, "",      "P_-3_m_1" },
    { "-P 3 2\"c",         165, "",      "P_-3_c_1" },
    { "-R 3 2\"",          166, "H",     "R_-3_m" },
    { "-P 3* 2",           166, "R",     "R_-3_m" },
    { "-R 3 2\"c",         167, "H",     "R_-3_c" },
    { "-P 3* 2n",          167, "R",     "R_-3_c" },
    { " P 6",              168, "",      "P_6" },
    { " P 61",             169, "",      "P_61" },
    { " P 65",             170, "",      "P_65" },
    { " P 62",             171, "",      "P_62" },
    { " P 64",             172, "",      "P_64" },
    { " P 6c",             173, "",      "P_63" },
    { " P -6",             174, "",      "P_-6" },
    { "-P 6",              175, "",      "P_6/m" },
    { "-P 6c",             176, "",      "P_63/m" },
    { " P 6 2",            177, "",      "P_6_2_2" },
    { " P 61 2 (0 0 -1)",  178, "",      "P_61_2_2" },
    { " P 65 2 (0 0 1)",   179, "",      "P_65_2_2" },
    { " P 62 2c (0 0 1)",  180, "",      "P_62_2_2" },
    { " P 64 2c (0 0 -1)", 181, "",      "P_64_2_2" },
    { " P 6c 2c",          182, "",      "P_63_2_2" },
    { " P 6 -2",           183, "",      "P_6_m_m" },
    { " P 6 -2c",          184, "",      "P_6_c_c" },
    { " P 6c -2",          185, "",      "P_63_c_m" },
    { " P 6c -2c",         186, "",      "P_63_m_c" },
    { " P -6 2",           187, "",      "P_-6_m_2" },
    { " P -6c 2",          188, "",      "P_-6_c_2" },
    { " P -6 -2",          189, "",      "P_-6_2_m" },
    { " P -6c -2c",        190, "",      "P_-6_2_c" },
    { "-P 6 2",            191, "",      "P_6/m_m_m" },
    { "-P 6 2c",           192, "",      "P_6/m_c_c" },
    { "-P 6c 2",           193, "",      "P_63/m_c_m" },
    { "-P 6c 2c",          194, "",      "P_63/m_m_c" },
    { " P 2 2 3",          195, "",      "P_2_3" },
    { " F 2 2 3",          196, "",      "F_2_3" },
    { " I 2 2 3",          197, "",      "I_2_3" },
    { " P 2ac 2ab 3",      198, "",      "P_21_3" },
    { " I 2b 2c 3",        199, "",      "I_21_3" },
    { "-P 2 2 3",          200, "",      "P_m_-3" },
    { " P 2 2 3 -1n",      201, "1",     "P_n_-3" },
    { "-P 2ab 2bc 3",      201, "2",     "P_n_-3" },
    { "-F 2 2 3",          202, "",      "F_m_-3" },
    { " F 2 2 3 -1d",      203, "1",     "F_d_-3" },
    { "-F 2uv 2vw 3",      203, "2",     "F_d_-3" },
    { "-I 2 2 3",          204, "",      "I_m_-3" },
    { "-P 2ac 2ab 3",      205, "",      "P_a_-3" },
    { "-I 2b 2c 3",        206, "",      "I_a_-3" },
    { " P 4 2 3",          207, "",      "P_4_3_2" },
    { " P 4n 2 3",         208, "",      "P_42_3_2" },
    { " F 4 2 3",          209, "",      "F_4_3_2" },
    { " F 4d 2 3",         210, "",      "F_41_3_2" },
    { " I 4 2 3",          211, "",      "I_4_3_2" },
    { " P 4acd 2ab 3",     212, "",      "P_43_3_2" },
    { " P 4bd 2ab 3",      213, "",      "P_41_3_2" },
    { " I 4bd 2c 3",       214, "",      "I_41_3_2" },
    { " P -4 2 3",         215, "",      "P_-4_3_m" },
    { " F -4 2 3",         216, "",      "F_-4_3_m" },
    { " I -4 2 3",         217, "",      "I_-4_3_m" },
    { " P -4n 2 3",        218, "",      "P_-4_3_n" },
    { " F -4c 2 3",        219, "",      "F_-4_3_c" },
    { " I -4bd 2c 3",      220, "",      "I_-4_3_d" },
    { "-P 4 2 3",          221, "",      "P_m_-3_m" },
    { " P 4 2 3 -1n",      222, "1",     "P_n_-3_n" },
    { "-P 4a 2bc 3",       222, "2",     "P_n_-3_n" },
    { "-P 4n 2 3",         223, "",      "P_m_-3_n" },
    { " P 4n 2 3 -1n",     224, "1",     "P_n_-3_m" },
    { "-P 4bc 2bc 3",      224, "2",     "P_n_-3_m" },
    { "-F 4 2 3",          225, "",      "F_m_-3_m" },
    { "-F 4c 2 3",         226, "",      "F_m_-3_c" },
    { " F 4d 2 3 -1d",     227, "1",     "F_d_-3_m" },
    { "-F 4vw 2vw 3",      227, "2",     "F_d_-3_m" },
    { " F 4d 2 3 -1cd",    228, "1",     "F_d_-3_c" },
    { "-F 4cvw 2vw 3",     228, "2",     "F_d_-3_c" },
    { "-I 4 2 3",          229, "",      "I_m_-3_m" },
    { "-I 4bd 2c 3",       230, "",      "I_a_-3_d" },
    { NULL, 0, NULL, NULL }
  };
#endif /* SGCLIB_C__ */


#define Sg_nLoopInv(SgInfo_)\
  ((SgInfo_)->Centric == -1 ? 2 : 1)


#define InitRotMx(RotMx, diagonal)\
  {\
    int  private_i_;\
    for (private_i_ = 0; private_i_ <  9; private_i_++)\
        (RotMx)[private_i_] = (private_i_ % 4 ? 0 : diagonal);\
  }

#define InitSeitzMx(SeitzMx_, diagonal)\
  {\
    int  private_i_;\
    for (private_i_ = 0; private_i_ < 12; private_i_++)\
      (SeitzMx_)->a[private_i_] = (private_i_ % 4 ? 0 : diagonal);\
  }


#if defined(SGCLIB_C__) || defined(SGCOREDEF__)
#define SpecialSMx_Identity       0x01
#define SpecialSMx_Inversion      0x02
#define SpecialSMx_Transl0        0x04
#endif


/* sgclib.c */

void SetSgError(const char *msg);
int iModPositive(int ix, int iy);
int traceRotMx(const int *RotMx);
int deterRotMx(const int *RotMx);
void RotMx_t_Vector(int *R_t_V, const int *RotMx, const int *Vector, int FacTr);
void RotMxMultiply(int *rmxab, const int *rmxa, const int *rmxb);
void RotateRotMx(int *RotMx, const int *RMx, const int *InvRMx);
void SeitzMxMultiply(T_RTMx *smxab, const T_RTMx *smxa, const T_RTMx *smxb);
void RTMxMultiply(T_RTMx *rtmxab, const T_RTMx *rtmxa, const T_RTMx *rtmxb,
                  int FacAug, int FacTr);
void InverseRotMx(const int *RotMx, int *InvRotMx);
void InverseRTMx(const T_RTMx *RTMx, T_RTMx *InvRTMx);
int IsSMxTransl0(const T_LatticeInfo *LatticeInfo, const int *SeitzMxT);
int CompareSeitzMx(const T_LatticeInfo *LatticeInfo,
                   const T_RTMx *SeitzMxA, const T_RTMx *SeitzMxB);
int GetRotMxOrder(const int *RotMx);
int GetRotMxInfo(const int *RotMx, T_RotMxInfo *RotMxInfo);
const T_RotMxInfo *ListOrBufRotMxInfo(const T_SgInfo *SgInfo, int iList,
                                      T_RotMxInfo *BufRotMxInfo);
int Add2ListSeitzMx(T_SgInfo *SgInfo, const T_RTMx *NewSMx);
int AddInversion2ListSeitzMx(T_SgInfo *SgInfo);
int AddLatticeTr2ListSeitzMx(T_SgInfo *SgInfo,
                             const T_LatticeInfo *LatticeInfo);
int ApplyOriginShift(T_SgInfo *SgInfo);
int FindSeitzMx(const T_SgInfo *SgInfo,
                int Order, int HonorSign, int RefAxis, int DirCode);
void InitSgInfo(T_SgInfo *SgInfo);
int CompleteSgInfo(T_SgInfo *SgInfo);
int CB_SMx(T_RTMx *CSiC,
           const T_RTMx *CBMx, const T_RTMx *SMx, const T_RTMx *InvCBMx);
int TransformSgInfo(const T_SgInfo *SgInfo,
                    const T_RTMx *CBMx, const T_RTMx *InvCBMx,
                    T_SgInfo *BC_SgInfo);

/* sgio.c */

const T_TabSgName *FindTabSgNameEntry(const char *UserSgName, int VolLetter);
unsigned int SgID_Number(const T_TabSgName *tsgn);
int ParseSymXYZ(const char *SymXYZ, T_RTMx *SeitzMx, int FacTr);
int ParseHallSymbol(const char *hsym, T_SgInfo *SgInfo);
int PrintFullHM_SgName(const T_TabSgName *tsgn, int space, FILE *fpout);
void PrintTabSgNameEntry(const T_TabSgName *tsgn, int Style, int space,
                         FILE *fpout);
const char *FormatFraction(int nume, int deno, int Decimal,
                           char *Buffer, int SizeBuffer);
const char *RTMx2XYZ(const T_RTMx *RTMx, int FacRo, int FacTr,
                     int Decimal, int TrFirst, int Low,
                     const char *Seperator,
                     char *BufferXYZ, int SizeBufferXYZ);
void PrintMapleRTMx(const T_RTMx *RTMx, int FacRo, int FacTr,
                    const char *Label, FILE *fpout);
void ListSgInfo(const T_SgInfo *SgInfo, int F_XYZ, int F_Verbose, FILE *fpout);


/* sgfind.c */

const T_TabSgName *FindReferenceSpaceGroup(T_SgInfo *SgInfo,
                                           T_RTMx *CBMx, T_RTMx *InvCBMx);

/* sghkl.c */

int IsSysAbsent_hkl(const T_SgInfo *SgInfo,
                    int h, int k, int l, int *TH_Restriction);
int BuildEq_hkl(const T_SgInfo *SgInfo, T_Eq_hkl *Eq_hkl, int h, int k, int l);
int AreSymEquivalent_hkl(const T_SgInfo *SgInfo, int h1, int k1, int l1,
                                                 int h2, int k2, int l2);
void SetListMin_hkl(const T_SgInfo *SgInfo,            int  Maxk, int  Maxl,
                                            int *Minh, int *Mink, int *Minl);
int IsSuppressed_hkl(const T_SgInfo *SgInfo, int Minh, int Mink, int Minl,
                                                       int Maxk, int Maxl,
                                             int    h, int    k, int    l);

/* sgsi.c */

void MarkLegalOrigins(const T_SgInfo *SgInfo, int *TestField);
int Verify_si(int h, int k, int l, const int *TestField);
int Is_si(const T_SgInfo *SgInfo, int h, int k, int l);
int Set_si(T_SgInfo *SgInfo);
void Set_uvw(const T_SgInfo *SgInfo, int h, int k, int l, int *uvw);


#endif /* SGINFO_H__ */
