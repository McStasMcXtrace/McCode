/*
  Space Group Info's (c) 1994-96 Ralf W. Grosse-Kunstleve
 */

#include <stdio.h>
#include <stdlib.h>


#include "sginfo.h"


static const char *IErr_Inc_SymMx =
  "Internal Error: Inconsistent symmetry matrices";


int IsSysAbsent_hkl(const T_SgInfo *SgInfo,
                    int h, int k, int l, int *TH_Restriction)
{
  int           iTrV, nTrV;
  const int     *TrV;
  int           iList, mh, mk, ml, hm, km, lm;
  int           TH, THr, FlagMismatch;
  const T_RTMx  *lsmx;


  mh = -h;
  mk = -k;
  ml = -l;

  /* check list of symmetry operations
     take care of lattice type and "centric" flag */

  THr = -1;
  if (TH_Restriction != NULL) *TH_Restriction = THr;
  FlagMismatch = 0;

  nTrV = SgInfo->LatticeInfo->nTrVector;
  lsmx = SgInfo->ListSeitzMx;

  for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
  {
    hm = lsmx->s.R[0] * h + lsmx->s.R[3] * k + lsmx->s.R[6] * l;
    km = lsmx->s.R[1] * h + lsmx->s.R[4] * k + lsmx->s.R[7] * l;
    lm = lsmx->s.R[2] * h + lsmx->s.R[5] * k + lsmx->s.R[8] * l;

    TrV = SgInfo->LatticeInfo->TrVector;

    for (iTrV = 0; iTrV < nTrV; iTrV++)
    {
      TH =  (lsmx->s.T[0] + *TrV++) * h;
      TH += (lsmx->s.T[1] + *TrV++) * k;
      TH += (lsmx->s.T[2] + *TrV++) * l;
      TH %= STBF; if (TH < 0) TH += STBF;

      if      (mh == hm && mk == km && ml == lm)
      {
        if (TH != 0 && SgInfo->Centric == -1)
          return -(iList + 1 + iTrV * SgInfo->nList);

        if (THr < 0)
          THr = TH;
        else if (THr != TH)
          FlagMismatch = 1; /* must be systematic absent */
                            /* will check later ...      */
      }
      else if ( h == hm &&  k == km &&  l == lm)
      {
        if (TH != 0)
          return  (iList + 1 + iTrV * SgInfo->nList);
      }
      else
        break;
    }
  }

  if (THr >= 0 && FlagMismatch) /* ... consistency check */
    SetSgError(IErr_Inc_SymMx);

  if (TH_Restriction != NULL)
  {
    if (SgInfo->Centric == -1) *TH_Restriction = 0;
    else                       *TH_Restriction = THr;
  }

  return 0;
}


int BuildEq_hkl(const T_SgInfo *SgInfo, T_Eq_hkl *Eq_hkl, int h, int k, int l)
{
  int       iList, hm, km, lm, i;
  T_RTMx    *lsmx;
  T_Eq_hkl  BufEq_hkl;


  if (Eq_hkl == NULL)
    Eq_hkl = &BufEq_hkl;

  Eq_hkl->M = 1;
  Eq_hkl->N = 1;
  Eq_hkl->h[0] = h;
  Eq_hkl->k[0] = k;
  Eq_hkl->l[0] = l;
  Eq_hkl->TH[0] = 0;

  if (! (h || k || l))
    return Eq_hkl->M; /* this is 000 */

  Eq_hkl->M++;

  /* check list of symmetry operations */

  lsmx = &SgInfo->ListSeitzMx[1]; /* skip first = identity matrix */

  for (iList = 1; iList < SgInfo->nList; iList++, lsmx++)
  {
    hm = lsmx->s.R[0] * h + lsmx->s.R[3] * k + lsmx->s.R[6] * l;
    km = lsmx->s.R[1] * h + lsmx->s.R[4] * k + lsmx->s.R[7] * l;
    lm = lsmx->s.R[2] * h + lsmx->s.R[5] * k + lsmx->s.R[8] * l;

    for (i = 0; i < Eq_hkl->N; i++)
    {
      if ( ( hm == Eq_hkl->h[i] &&  km == Eq_hkl->k[i] &&  lm == Eq_hkl->l[i])
        || (-hm == Eq_hkl->h[i] && -km == Eq_hkl->k[i] && -lm == Eq_hkl->l[i]))
        break;
    }

    if (i == Eq_hkl->N)
    {
      if (Eq_hkl->N >= 24) {
        SetSgError(IErr_Inc_SymMx);
        return 0;
      }

      Eq_hkl->h[i] = hm;
      Eq_hkl->k[i] = km;
      Eq_hkl->l[i] = lm;

          Eq_hkl->TH[i] = (  lsmx->s.T[0] * h
                           + lsmx->s.T[1] * k
                           + lsmx->s.T[2] * l) % STBF;
      if (Eq_hkl->TH[i] < 0)
          Eq_hkl->TH[i] += STBF;

      Eq_hkl->M += 2;
      Eq_hkl->N++;
    }
  }

  if (SgInfo->nList % Eq_hkl->N) /* another error trap */ {
    SetSgError(IErr_Inc_SymMx);
    return 0;
  }

  return Eq_hkl->M;
}


int AreSymEquivalent_hkl(const T_SgInfo *SgInfo, int h1, int k1, int l1,
                                                 int h2, int k2, int l2)
{
  int     iList, mh2, mk2, ml2, hm, km, lm;
  T_RTMx  *lsmx;


  mh2 = -h2;
  mk2 = -k2;
  ml2 = -l2;

  /* check list of symmetry operations */

  lsmx = SgInfo->ListSeitzMx;

  for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
  {
    hm = lsmx->s.R[0] * h1 + lsmx->s.R[3] * k1 + lsmx->s.R[6] * l1;
    km = lsmx->s.R[1] * h1 + lsmx->s.R[4] * k1 + lsmx->s.R[7] * l1;
    lm = lsmx->s.R[2] * h1 + lsmx->s.R[5] * k1 + lsmx->s.R[8] * l1;

    if      ( h2 == hm &&  k2 == km &&  l2 == lm)
      return  (iList + 1);

    else if (mh2 == hm && mk2 == km && ml2 == lm)
      return -(iList + 1);
  }

  return 0;
}


void SetListMin_hkl(const T_SgInfo *SgInfo,            int  Maxk, int  Maxl,
                                            int *Minh, int *Mink, int *Minl)
{
  *Minh = 0;

  switch(SgInfo->XtalSystem)
  {
    case XS_Triclinic:
      *Mink = -Maxk;
      *Minl = -Maxl;
      break;
    case XS_Monoclinic:
      if (SgInfo->UniqueRefAxis == 'z')
      {
        *Mink = -Maxk;
        *Minl = 0;
      }
      else
      {
        *Mink = 0;
        *Minl = -Maxl;
      }
      break;
    default:
      if (SgInfo->XtalSystem == XS_Trigonal && SgInfo->UniqueDirCode == '*')
        *Mink = -Maxk;
      else
        *Mink = 0;
      *Minl = 0;
      break;
  }
}


int IsSuppressed_hkl(const T_SgInfo *SgInfo, int Minh, int Mink, int Minl,
                                                       int Maxk, int Maxl,
                                             int    h, int    k, int    l)
{
  int     iList, mate, hm, km, lm;
  T_RTMx  *lsmx;


  /* check for Friedel mate first */

  hm = -h, km = -k, lm = -l;

  if (   (Minh <= hm && hm <=    h)
      && (Mink <= km && km <= Maxk)
      && (Minl <= lm && lm <= Maxl))
  {
    if (hm < h) return -1;
    else /* if (h == 0) */
      if (km < k) return -1;
      else if (k == 0)
        if (lm < l) return -1;
  }
  lsmx = &SgInfo->ListSeitzMx[1]; /* skip first = identity matrix */

  for (iList = 1; iList < SgInfo->nList; iList++, lsmx++)
  {
    /* check if equivalent hm, km, lm are inside loop range ... */

    hm = lsmx->s.R[0] * h + lsmx->s.R[3] * k + lsmx->s.R[6] * l;
    km = lsmx->s.R[1] * h + lsmx->s.R[4] * k + lsmx->s.R[7] * l;
    lm = lsmx->s.R[2] * h + lsmx->s.R[5] * k + lsmx->s.R[8] * l;

    for (mate = 0; mate < 2; mate++)
    {
      if (mate) hm = -hm, km = -km, lm = -lm; /* ... or friedel mate */

      if (   Minh <= hm && hm <=    h
          && Mink <= km && km <= Maxk
          && Minl <= lm && lm <= Maxl)
      {
        /* ... and were processed before */

        if (hm < h)
          return (mate ? -(iList + 1) : iList + 1);
        else /* if (hm == h) */
          if (km < k)
            return (mate ? -(iList + 1) : iList + 1);
          else if (km == k)
            if (lm < l)
              return (mate ? -(iList + 1) : iList + 1);
      }
    }
  }

  return 0;
}
