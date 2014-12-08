/*
  Space Group Info's (c) 1994-96 Ralf W. Grosse-Kunstleve
 */

#include <stdio.h>
#include <stdlib.h>


#ifdef APP_INCLUDE
#include APP_INCLUDE
#endif

#ifndef AppMalloc
#define AppMalloc(ptr, n) (ptr) = malloc((n) * sizeof (*(ptr)))
#endif
#ifndef AppFree
#define AppFree(ptr, n) free(ptr)
#endif


#include "sginfo.h"


/* Non elegant way to get s.i. vectors and moduli:
     1. Build field with legal reference points marked (TestField)
     2. Go through list of possible s.i. vects and mods:
        Verify with TestField
 */


void MarkLegalOrigins(const T_SgInfo *SgInfo, int *TestField)
{
  int           O[3], V[3], lx, ly, lz, mx, my, mz, i;
  int           IsFine, iList, iLoopInv, nLoopInv;
  int           BufMx[9];
  const T_RTMx  *lsmx;
  int           nTrV, iTrV;
  const int     *TrV;


  nLoopInv = Sg_nLoopInv(SgInfo);

  nTrV = SgInfo->LatticeInfo->nTrVector;

  switch (SgInfo->LatticeInfo->Code)
  {
    default:
    case 'P': lx = ly = lz = 12;    break;
    case 'A': lx = ly = 12; lz = 6; break;
    case 'B': ly = lz = 12; lx = 6; break;
    case 'C': lz = lx = 12; ly = 6; break;
    case 'I': lx = ly = 12; lz = 6; break;
    case 'R': lx = ly = 12; lz = 4; break;
    case 'S': lz = lx = 12; ly = 4; break;
    case 'T': ly = lz = 12; lx = 4; break;
    case 'F': lx = 12; ly = lz = 6; break;
  }

  for (O[0] = 0; O[0] < 12; O[0]++)
  for (O[1] = 0; O[1] < 12; O[1]++)
  for (O[2] = 0; O[2] < 12; O[2]++)
  {
    IsFine = 1;

    for (iList = 0; IsFine && iList < SgInfo->nList; iList++)
    {
      lsmx = &SgInfo->ListSeitzMx[iList];

      for (iLoopInv = 0; IsFine && iLoopInv < nLoopInv; iLoopInv++)
      {
        if (iLoopInv == 0)
          for (i = 0; i < 9; i++)
          {
            if (i % 4) BufMx[i] =  lsmx->s.R[i];
            else       BufMx[i] =  lsmx->s.R[i] - 1;
          }
        else
          for (i = 0; i < 9; i++)
          {
            if (i % 4) BufMx[i] = -lsmx->s.R[i];
            else       BufMx[i] = -lsmx->s.R[i] - 1;
          }

        RotMx_t_Vector(V, BufMx, O, 12);

        TrV = SgInfo->LatticeInfo->TrVector;

        for (iTrV = 0; iTrV < nTrV; iTrV++)
        {
          mx = (V[0] * (STBF / 12) + *TrV++) % STBF;
          my = (V[1] * (STBF / 12) + *TrV++) % STBF;
          mz = (V[2] * (STBF / 12) + *TrV++) % STBF;

          if (mx == 0 && my == 0 && mz == 0)
            break;
        }

        if (iTrV == nTrV) IsFine = 0;
      }
    }

    if (! (O[0] < lx && O[1] < ly && O[2] < lz))
      IsFine = -IsFine;

    *TestField++ = IsFine;

#if DEBUG_MarkLegalOrigins
    if      (IsFine ==  1) putc(' ', stdout);
    else if (IsFine == -1) putc('#', stdout);
    if (IsFine != 0)
      fprintf(stdout, " %2d %2d %2d\n", O[0], O[1], O[2]);
#endif
  }
}


#define IsArbitraryShift(iShift) \
  (    (iShift) == 1 || (iShift) ==  5 \
    || (iShift) == 7 || (iShift) == 11)


int Verify_si(int h, int k, int l, const int *TestField)
{
  int    O[3], TH;


  for (O[0] = 0; O[0] < 12; O[0]++)
  for (O[1] = 0; O[1] < 12; O[1]++)
  for (O[2] = 0; O[2] < 12; O[2]++)
  {
    if (*TestField++)
    {
          TH = h * O[0] + k * O[1] + l * O[2];
          TH %= 12;
      if (TH) return 0;

      if (IsArbitraryShift(O[0])) TH += h;
      if (IsArbitraryShift(O[1])) TH += k;
      if (IsArbitraryShift(O[2])) TH += l;
      if (TH) return 0;
    }
  }

  return 1;
}


int Is_si(const T_SgInfo *SgInfo, int h, int k, int l)
{
  int        i_si_v, u;
  const int  *si_v, *si_m;


  si_v = SgInfo->si_Vector;
  si_m = SgInfo->si_Modulus;

  for (i_si_v = 0; i_si_v < SgInfo->n_si_Vector; i_si_v++)
  {
    u =  *si_v++ * h;
    u += *si_v++ * k;
    u += *si_v++ * l;

    if (*si_m) {
      if (u % (*si_m)) return 0; }
    else {
      if (u)           return 0; }

    si_m++;
  }

  return 1;
}


int Set_si(T_SgInfo *SgInfo)
{
  static const int TabTrial_si[] =
    {
      0,

      1,   0,  2, -1,  4,  /* I -4 */
      1,   2, -1,  0,  4,
      1,  -1,  0,  2,  4,

      1,   2,  4,  3,  6,  /* P 3 2 */
      1,   4,  3,  2,  6,
      1,   3,  2,  4,  6,

      1,   1,  1,  1,  4,
      1,   1,  1,  1,  2,
      1,   1,  1,  1,  0,

      1,   0,  0,  1,  2,
      1,   0,  1,  0,  2,
      1,   1,  0,  0,  2,

      1,   0,  0,  1,  0,
      1,   0,  1,  0,  0,
      1,   1,  0,  0,  0,

      2,   1, -1,  0,  3,
           0,  0,  1,  0,
      2,  -1,  0,  1,  3,
           0,  1,  0,  0,
      2,   0,  1, -1,  3,
           1,  0,  0,  0,

      2,   0,  1,  1,  4,  /* F 2x */
           1,  0,  0,  0,
      2,   1,  0,  1,  4,  /* F 2y */
           0,  1,  0,  0,
      2,   1,  1,  0,  4,  /* F 2z */
           0,  0,  1,  0,

      2,   1,  0,  0,  2,
           0,  0,  1,  2,
      2,   0,  1,  0,  2,
           0,  0,  1,  2,
      2,   1,  0,  0,  2,
           0,  1,  0,  2,

      2,   1,  1,  0,  2,
           0,  0,  1,  2,
      2,   1,  0,  1,  2,
           0,  1,  0,  2,
      2,   0,  1,  1,  2,
           1,  0,  0,  2,

      2,   1,  0,  0,  2,
           0,  0,  1,  0,
      2,   0,  1,  0,  2,
           0,  0,  1,  0,
      2,   1,  0,  0,  2,
           0,  1,  0,  0,

      2,   1,  0,  0,  0,
           0,  0,  1,  2,
      2,   0,  1,  0,  0,
           0,  0,  1,  2,
      2,   1,  0,  0,  0,
           0,  1,  0,  2,

      2,   1,  1,  0,  2,
           0,  0,  1,  0,
      2,   1,  0,  1,  2,
           0,  1,  0,  0,
      2,   0,  1,  1,  2,
           1,  0,  0,  0,

      2,   1,  0,  0,  0,
           0,  0,  1,  0,
      2,   0,  1,  0,  0,
           0,  0,  1,  0,
      2,   1,  0,  0,  0,
           0,  1,  0,  0,

      3,   1,  0,  0,  2,
           0,  1,  0,  2,
           0,  0,  1,  2,

      3,   1,  0,  0,  0,
           0,  1,  0,  2,
           0,  0,  1,  2,

      3,   1,  0,  0,  2,
           0,  1,  0,  0,
           0,  0,  1,  2,

      3,   1,  0,  0,  2,
           0,  1,  0,  2,
           0,  0,  1,  0,

      3,   1,  0,  0,  2,
           0,  1,  0,  0,
           0,  0,  1,  0,

      3,   1,  0,  0,  0,
           0,  1,  0,  2,
           0,  0,  1,  0,

      3,   1,  0,  0,  0,
           0,  1,  0,  0,
           0,  0,  1,  2,

      3,   1,  0,  0,  0,
           0,  1,  0,  0,
           0,  0,  1,  0,

      3,  -1,  0,  0,  2,  /* -A 1 */
           0, -1,  1,  4,
           0,  1,  1,  4,

      3,  -1,  0,  1,  4,  /* -B 1 */
           0, -1,  0,  2,
           1,  0,  1,  4,

      3,   1,  1,  0,  4,  /* -C 1 */
           1, -1,  0,  4,
           0,  0, -1,  2,

      3,  -1,  1,  1,  4,  /* -I 1 */
           1, -1,  1,  4,
           1,  1, -1,  4,

      3,   0,  1,  1,  4,  /* -F 1 */
           1,  0,  1,  4,
           1,  1,  0,  4,

      3,  -1,  0,  0,  0,  /* A 2x */
           0, -1,  1,  4,
           0,  1,  1,  4,

      3,  -1,  0,  1,  4,  /* B 2y */
           0, -1,  0,  0,
           1,  0,  1,  4,

      3,   1,  1,  0,  4,  /* C 2z */
           1, -1,  0,  4,
           0,  0, -1,  0,

      -1
    };

  int        h, k, l, iList;
  int        Maxh, Maxk, Maxl;
  int        Minh, Mink, Minl;
  int        nTestField, *TestField;
  int        nProperty, *Property, *pp;
  int        IsFine, would_be, is;
  int        i_si, *si_v;
  const int  *trial_si;


  SgInfo->n_si_Vector = -1;

                       nTestField = 12 * 12 * 12;
  AppMalloc(TestField, nTestField);
  if (TestField == NULL) {
    SetSgError("Not enough core");
    return -1;
  }

  MarkLegalOrigins(SgInfo, TestField);

  Maxh = Maxk = Maxl = 7;
  SetListMin_hkl(SgInfo, Maxk, Maxl, &Minh, &Mink, &Minl);

  nProperty =   (Maxh - Minh + 1)
              * (Maxk - Mink + 1)
              * (Maxl - Minl + 1);
  AppMalloc(Property, nProperty);
  if (Property == NULL) {
    SetSgError("Not enough core");
    AppFree(TestField, nTestField);
    return -1;
  }

  pp = Property;
  for (h = Minh; h <= Maxh; h++)
  for (k = Mink; k <= Maxk; k++)
  for (l = Minl; l <= Maxl; l++)
  {
    iList = IsSysAbsent_hkl(SgInfo, h, k, l, NULL);
    if (SgError != NULL)
    {
      AppFree(Property, nProperty);
      AppFree(TestField, nTestField);
      return -1;
    }

    if (iList == 0)
      *pp++ = Verify_si(h, k, l, TestField);
    else
      *pp++ = -1;
  }

  trial_si = TabTrial_si;
  while (*trial_si >= 0)
  {
    SgInfo->n_si_Vector = *trial_si++;
    si_v = SgInfo->si_Vector;
    for (i_si = 0; i_si < SgInfo->n_si_Vector; i_si++)
    {
      *si_v++ = *trial_si++;
      *si_v++ = *trial_si++;
      *si_v++ = *trial_si++;
      SgInfo->si_Modulus[i_si] = *trial_si++;
    }

    IsFine = 1;

    pp = Property;
    for (h = Minh; IsFine && h <= Maxh; h++)
    for (k = Mink; IsFine && k <= Maxk; k++)
    for (l = Minl; IsFine && l <= Maxl; l++)
    {
      is = *pp++;

      if (is >= 0)
      {
        would_be = Is_si(SgInfo, h, k, l);
        if (is != would_be)
          IsFine = 0;
      }
    }

    if (IsFine)
    {
      AppFree(Property, nProperty);
      AppFree(TestField, nTestField);
      return 0;
    }
  }

  SgInfo->n_si_Vector = -1;
  SetSgError("Internal Error: Can't determine s.i. vectors and moduli");

  AppFree(Property, nProperty);
  AppFree(TestField, nTestField);

  return -1;
}


void Set_uvw(const T_SgInfo *SgInfo, int h, int k, int l, int *uvw)
{
  int        i_si_v, u;
  const int  *si_v, *si_m;


  si_v = SgInfo->si_Vector;
  si_m = SgInfo->si_Modulus;

  for (i_si_v = 0; i_si_v < SgInfo->n_si_Vector; i_si_v++)
  {
    u =  *si_v++ * h;
    u += *si_v++ * k;
    u += *si_v++ * l;

    if (*si_m) u %= (*si_m);
    si_m++;

    uvw[i_si_v] = u;
  }
}
