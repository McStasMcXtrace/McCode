/*
  Space Group Info's (c) 1994-96 Ralf W. Grosse-Kunstleve
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define SGCLIB_C__
#include "sginfo.h"


static const char *Err_Ill_SMx_in_List =
  "Error: Illegal SeitzMx in list";


void SetSgError(const char *msg)
{
  if (SgError == NULL) SgError = msg;
}


int iModPositive(int ix, int iy)
{
  if (iy > 0)
  {
    ix %= iy;
    if (ix < 0) ix += iy;
  }

  return ix;
}


static void SwapRTMx(T_RTMx *Mx_a, T_RTMx *Mx_b)
{
  int     i;
  T_RTMx  BufMx;


  for (i = 0; i < 12; i++) BufMx.a[i] = Mx_a->a[i];
  for (i = 0; i < 12; i++) Mx_a->a[i] = Mx_b->a[i];
  for (i = 0; i < 12; i++) Mx_b->a[i] = BufMx.a[i];
}


static void CopyRotMxInfo(T_RotMxInfo *target, const T_RotMxInfo *source)
{
  memcpy(target, source, sizeof (*target));
}


static void SwapRotMxInfo(T_RotMxInfo *RMx_a, T_RotMxInfo *RMx_b)
{
  T_RotMxInfo  Buffer;

  memcpy(&Buffer, RMx_a,   sizeof (Buffer));
  memcpy(RMx_a,   RMx_b,   sizeof (Buffer));
  memcpy(RMx_b,   &Buffer, sizeof (Buffer));
}


int traceRotMx(const int *RotMx)
{
  return RotMx[0] + RotMx[4] + RotMx[8];
}


int deterRotMx(const int *RotMx)
{
  int     det;

  det =  RotMx[0] * (RotMx[4] * RotMx[8] - RotMx[5] * RotMx[7]);
  det -= RotMx[1] * (RotMx[3] * RotMx[8] - RotMx[5] * RotMx[6]);
  det += RotMx[2] * (RotMx[3] * RotMx[7] - RotMx[4] * RotMx[6]);

  return det;
}


void RotMx_t_Vector(int *R_t_V, const int *RotMx, const int *Vector, int FacTr)
{
  const int  *vec;


  if (FacTr > 0)
  {
                            vec = Vector;
    *R_t_V   =  *RotMx++ * *vec++;
    *R_t_V   += *RotMx++ * *vec++;
    *R_t_V   += *RotMx++ * *vec;
    *R_t_V   %= FacTr; if (*R_t_V < 0) *R_t_V += FacTr;
     R_t_V++;
                            vec = Vector;
    *R_t_V   =  *RotMx++ * *vec++;
    *R_t_V   += *RotMx++ * *vec++;
    *R_t_V   += *RotMx++ * *vec;
    *R_t_V   %= FacTr; if (*R_t_V < 0) *R_t_V += FacTr;
     R_t_V++;
                            vec = Vector;
    *R_t_V   =  *RotMx++ * *vec++;
    *R_t_V   += *RotMx++ * *vec++;
    *R_t_V   += *RotMx   * *vec;
    *R_t_V   %= FacTr; if (*R_t_V < 0) *R_t_V += FacTr;
  }
  else
  {
                            vec = Vector;
    *R_t_V   =  *RotMx++ * *vec++;
    *R_t_V   += *RotMx++ * *vec++;
    *R_t_V++ += *RotMx++ * *vec;
                            vec = Vector;
    *R_t_V   =  *RotMx++ * *vec++;
    *R_t_V   += *RotMx++ * *vec++;
    *R_t_V++ += *RotMx++ * *vec;
                            vec = Vector;
    *R_t_V   =  *RotMx++ * *vec++;
    *R_t_V   += *RotMx++ * *vec++;
    *R_t_V   += *RotMx   * *vec;
  }
}


void RotMxMultiply(int *rmxab, const int *rmxa, const int *rmxb)
{
  const int  *a, *b;

  /* no loops to be as fast as posslible */

  a = rmxa;
  b = rmxb;
  *rmxab  = *a++ * *b; b += 3; /* r11 */
  *rmxab += *a++ * *b; b += 3;
  *rmxab += *a   * *b; b -= 5;
   rmxab++;

  a = rmxa;
  *rmxab  = *a++ * *b; b += 3; /* r12 */
  *rmxab += *a++ * *b; b += 3;
  *rmxab += *a   * *b; b -= 5;
   rmxab++;

  a = rmxa;
  *rmxab  = *a++ * *b; b += 3; /* r13 */
  *rmxab += *a++ * *b; b += 3;
  *rmxab += *a++ * *b; b = rmxb;
   rmxab++;

  rmxa = a;
  *rmxab  = *a++ * *b; b += 3; /* r21 */
  *rmxab += *a++ * *b; b += 3;
  *rmxab += *a   * *b; b -= 5;
   rmxab++;

  a = rmxa;
  *rmxab  = *a++ * *b; b += 3; /* r22 */
  *rmxab += *a++ * *b; b += 3;
  *rmxab += *a   * *b; b -= 5;
   rmxab++;

  a = rmxa;
  *rmxab  = *a++ * *b; b += 3; /* r23 */
  *rmxab += *a++ * *b; b += 3;
  *rmxab += *a++ * *b; b = rmxb;
   rmxab++;

  rmxa = a;
  *rmxab  = *a++ * *b; b += 3; /* r31 */
  *rmxab += *a++ * *b; b += 3;
  *rmxab += *a   * *b; b -= 5;
   rmxab++;

  a = rmxa;
  *rmxab  = *a++ * *b; b += 3; /* r32 */
  *rmxab += *a++ * *b; b += 3;
  *rmxab += *a   * *b; b -= 5;
   rmxab++;

  a = rmxa;
  *rmxab  = *a++ * *b; b += 3; /* r33 */
  *rmxab += *a++ * *b; b += 3;
  *rmxab += *a   * *b;
}


void RotateRotMx(int *RotMx, const int *RMx, const int *InvRMx)
{
  int  BufMx[9];


  RotMxMultiply(BufMx, RotMx, InvRMx);
  RotMxMultiply(RotMx, RMx,   BufMx);
}


void SeitzMxMultiply(T_RTMx *smxab, const T_RTMx *smxa, const T_RTMx *smxb)
{
  const int  *ar, *a, *b, *bt;
  int        *ab;

  /* no loops to be as fast as posslible */

  ar = smxa->a;
  a  = smxa->a;
  b  = smxb->a;
  ab = smxab->a;

  *ab  = *a++ * *b; b += 3; /* r11 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r12 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r13 */
  *ab += *a++ * *b; b += 3;
  *ab += *a++ * *b; b = smxb->a;
   ab++;

  ar = a;
  *ab  = *a++ * *b; b += 3; /* r21 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r22 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r23 */
  *ab += *a++ * *b; b += 3;
  *ab += *a++ * *b; b = smxb->a;
   ab++;

  ar = a;
  *ab  = *a++ * *b; b += 3; /* r31 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r32 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r33 */
  *ab += *a++ * *b; b += 3;
  *ab += *a++ * *b++; bt = b;
   ab++;

  ar = smxa->a;
  *ab  = *ar++ * *b++; /* t1 */
  *ab += *ar++ * *b++;
  *ab += *ar++ * *b; b = bt;
  *ab += *a++;
  *ab %= STBF; if (*ab < 0) *ab += STBF;
   ab++;

  *ab  = *ar++ * *b++; /* t2 */
  *ab += *ar++ * *b++;
  *ab += *ar++ * *b; b = bt;
  *ab += *a++;
  *ab %= STBF; if (*ab < 0) *ab += STBF;
   ab++;

  *ab  = *ar++ * *b++; /* t3 */
  *ab += *ar++ * *b++;
  *ab += *ar   * *b;
  *ab += *a;
  *ab %= STBF; if (*ab < 0) *ab += STBF;
}


void RTMxMultiply(T_RTMx *rtmxab, const T_RTMx *rtmxa, const T_RTMx *rtmxb,
                  int FacAug, int FacTr)
{
  const int  *ar, *a, *b, *bt;
  int        *ab;

  /* no loops to be as fast as posslible */

  ar = rtmxa->a;
  a  = rtmxa->a;
  b  = rtmxb->a;
  ab = rtmxab->a;

  *ab  = *a++ * *b; b += 3; /* r11 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r12 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r13 */
  *ab += *a++ * *b; b += 3;
  *ab += *a++ * *b; b = rtmxb->a;
   ab++;

  ar = a;
  *ab  = *a++ * *b; b += 3; /* r21 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r22 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r23 */
  *ab += *a++ * *b; b += 3;
  *ab += *a++ * *b; b = rtmxb->a;
   ab++;

  ar = a;
  *ab  = *a++ * *b; b += 3; /* r31 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r32 */
  *ab += *a++ * *b; b += 3;
  *ab += *a   * *b; b -= 5;
   ab++;

  a = ar;
  *ab  = *a++ * *b; b += 3; /* r33 */
  *ab += *a++ * *b; b += 3;
  *ab += *a++ * *b++; bt = b;
   ab++;

  if (FacTr > 0)
  {
    ar = rtmxa->a;
    *ab  = *ar++ * *b++; /* t1 */
    *ab += *ar++ * *b++;
    *ab += *ar++ * *b; b = bt;
    *ab += *a++ * FacAug;
    *ab %= FacTr; if (*ab < 0) *ab += FacTr;
     ab++;

    *ab  = *ar++ * *b++; /* t2 */
    *ab += *ar++ * *b++;
    *ab += *ar++ * *b; b = bt;
    *ab += *a++ * FacAug;
    *ab %= FacTr; if (*ab < 0) *ab += FacTr;
     ab++;

    *ab  = *ar++ * *b++; /* t3 */
    *ab += *ar++ * *b++;
    *ab += *ar   * *b;
    *ab += *a   * FacAug;
    *ab %= FacTr; if (*ab < 0) *ab += FacTr;
  }
  else
  {
    ar = rtmxa->a;
    *ab  = *ar++ * *b++; /* t1 */
    *ab += *ar++ * *b++;
    *ab += *ar++ * *b; b = bt;
    *ab += *a++ * FacAug;
     ab++;

    *ab  = *ar++ * *b++; /* t2 */
    *ab += *ar++ * *b++;
    *ab += *ar++ * *b; b = bt;
    *ab += *a++ * FacAug;
     ab++;

    *ab  = *ar++ * *b++; /* t3 */
    *ab += *ar++ * *b++;
    *ab += *ar   * *b;
    *ab += *a   * FacAug;
  }
}


void InverseRotMx(const int *RotMx, int *InvRotMx)
{
  InvRotMx[0] =   RotMx[4] * RotMx[8] - RotMx[5] * RotMx[7];
  InvRotMx[1] = - RotMx[1] * RotMx[8] + RotMx[2] * RotMx[7];
  InvRotMx[2] =   RotMx[1] * RotMx[5] - RotMx[2] * RotMx[4];
  InvRotMx[3] = - RotMx[3] * RotMx[8] + RotMx[5] * RotMx[6];
  InvRotMx[4] =   RotMx[0] * RotMx[8] - RotMx[2] * RotMx[6];
  InvRotMx[5] = - RotMx[0] * RotMx[5] + RotMx[2] * RotMx[3];
  InvRotMx[6] =   RotMx[3] * RotMx[7] - RotMx[4] * RotMx[6];
  InvRotMx[7] = - RotMx[0] * RotMx[7] + RotMx[1] * RotMx[6];
  InvRotMx[8] =   RotMx[0] * RotMx[4] - RotMx[1] * RotMx[3];
}


void InverseRTMx(const T_RTMx *RTMx, T_RTMx *InvRTMx)
{
  int        *iR;
  const int  *T;


  iR = InvRTMx->s.R;

  InverseRotMx(RTMx->s.R, iR);

  T = RTMx->s.T;

  InvRTMx->s.T[0] = - iR[0] * T[0] - iR[1] * T[1] - iR[2] * T[2];
  InvRTMx->s.T[1] = - iR[3] * T[0] - iR[4] * T[1] - iR[5] * T[2];
  InvRTMx->s.T[2] = - iR[6] * T[0] - iR[7] * T[1] - iR[8] * T[2];
}


int IsSMxTransl0(const T_LatticeInfo *LatticeInfo, const int *SeitzMxT)
{
  int        iTrV, nTrV, t;
  const int  *TrV;


  nTrV = LatticeInfo->nTrVector;
   TrV = LatticeInfo->TrVector;

  for (iTrV = 0; iTrV < nTrV; iTrV++)
  {
        t =     (SeitzMxT[0] + TrV[0]) % STBF;
    if (t == 0) {
          t =   (SeitzMxT[1] + TrV[1]) % STBF;
      if (t == 0) {
            t = (SeitzMxT[2] + TrV[2]) % STBF;
        if (t == 0)
          return 1;
    }}

    TrV += 3;
  }

  return 0;
}


static int IsSpecialSeitzMx(T_SgInfo *SgInfo, const T_RTMx *SMx, int ExpandLT)
{
  int                  i, special, smx11;
  const T_LatticeInfo  *ExpLT;


  /* check rotation part for identity or inversion operation */

         smx11 = SMx->s.R[0];
  if (   smx11 !=  1
      && smx11 != -1) return 0;

  for (i = 1; i < 9; i++)
  {
    if (i % 4) {
      if (SMx->s.R[i] !=     0) return 0; }
    else {
      if (SMx->s.R[i] != smx11) return 0; }
  }

  if (smx11 == 1) special = SpecialSMx_Identity;
  else            special = SpecialSMx_Inversion;

  /* rotation part is identity or inversion
     check translation part now
   */

  if (IsSMxTransl0(SgInfo->LatticeInfo, SMx->s.T) == 1)
    return (special | SpecialSMx_Transl0);

  if (ExpandLT && (smx11 == 1 || SgInfo->Centric))
  {
    /* try to expand lattice type */

    ExpLT = NULL;

    switch (SgInfo->LatticeInfo->Code)
    {
      case 'P':
        if (IsSMxTransl0(LI_A, SMx->s.T) == 1)
        { ExpLT = LI_A; break; }
        if (IsSMxTransl0(LI_B, SMx->s.T) == 1)
        { ExpLT = LI_B; break; }
        if (IsSMxTransl0(LI_C, SMx->s.T) == 1)
        { ExpLT = LI_C; break; }
        if (IsSMxTransl0(LI_I, SMx->s.T) == 1)
        { ExpLT = LI_I; break; }
        if (IsSMxTransl0(LI_R, SMx->s.T) == 1)
        { ExpLT = LI_R; break; }
        if (IsSMxTransl0(LI_S, SMx->s.T) == 1)
        { ExpLT = LI_S; break; }
        if (IsSMxTransl0(LI_T, SMx->s.T) == 1)
        { ExpLT = LI_T; break; }
      case 'A':
      case 'B':
      case 'C':
        if (IsSMxTransl0(LI_F, SMx->s.T) == 1)
          ExpLT = LI_F;
    }

    if (ExpLT != NULL)
    {
      SgInfo->LatticeInfo = ExpLT;
      SgInfo->StatusLatticeTr = -1;
      return (special | SpecialSMx_Transl0);
    }
  }

  return special;
}


int CompareSeitzMx(const T_LatticeInfo *LatticeInfo,
                   const T_RTMx *SeitzMxA, const T_RTMx *SeitzMxB)
{
  int        i, iTrV, nTrV, t;
  const int  *TrV;


  /* compare rotation part */

  for (i = 0; i < 9; i++)
    if (SeitzMxA->s.R[i] != SeitzMxB->s.R[i]) return 1;

  /* rotation part is same
     check translation
   */

  nTrV = LatticeInfo->nTrVector;
   TrV = LatticeInfo->TrVector;

  for (iTrV = 0; iTrV < nTrV; iTrV++, TrV += 3)
  {
        t =     (SeitzMxA->s.T[0] + TrV[0]) % STBF;
    if (t ==     SeitzMxB->s.T[0]) {
          t =   (SeitzMxA->s.T[1] + TrV[1]) % STBF;
      if (t ==   SeitzMxB->s.T[1]) {
            t = (SeitzMxA->s.T[2] + TrV[2]) % STBF;
        if (t == SeitzMxB->s.T[2])
          return 0;
    }}
  }

  return -1;
}


int GetRotMxOrder(const int *RotMx)
{
  int deter = deterRotMx(RotMx);

  if (deter == -1 || deter == 1)
  {
    switch (traceRotMx(RotMx))
    {
      case -3:                  return -1;
      case -2:                  return -6;
      case -1: if (deter == -1) return -4;
               else             return  2;
      case  0: if (deter == -1) return -3;
               else             return  3;
      case  1: if (deter == -1) return -2;
               else             return  4;
      case  2:                  return  6;
      case  3:                  return  1;
    }
  }

  return 0;
}


static int nNextBasis_of_DirCode(const int DirCode,
                                 const int **RMx, const int **InvRMx)
{
  switch (DirCode)
  {
    case  '.': *RMx =            *InvRMx = NULL;      return 1;
    case  '=':
    case  '"':
    case '\'':
    case  '|':
    case '\\': *RMx = RMx_3_111; *InvRMx = RMx_3i111; return 3;
    case  '*': *RMx = RMx_4_001; *InvRMx = RMx_4i001; return 4;
    default:
      break;
  }

  SetSgError("Internal Error: Corrupt DirCode");
  return -1;
}


int GetRotMxInfo(const int *RotMx, T_RotMxInfo *RotMxInfo)
{
  int        i;
  int        nNextBasis, iNextBasis;
  int        nLoopInv, iLoopInv;
  int        Order, AbsOrder;
  int        RMxCopy[9], MatchMx[9], InvMatchMx[9], REV[3];
  int        *mmx;
  const int  *NBRMx, *InvNBRMx;

  const T_TabXtalRotMx  *txrmx;


  Order = GetRotMxOrder(RotMx);

  if (RotMxInfo)
      RotMxInfo->Order = Order;

  if (Order)
  {
    AbsOrder = abs(Order);

    if (Order > 0)
      for (i = 0; i < 9; i++) RMxCopy[i] =  RotMx[i];
    else
      for (i = 0; i < 9; i++) RMxCopy[i] = -RotMx[i];

    for (txrmx = TabXtalRotMx; txrmx->Order; txrmx++)
      if (txrmx->Order == AbsOrder) break;

    while (txrmx->Order == AbsOrder)
    {
      nNextBasis = nNextBasis_of_DirCode(txrmx->DirCode, &NBRMx, &InvNBRMx);

      if (nNextBasis < 0)
        return 0;

      if (AbsOrder > 2) nLoopInv = 2;
      else              nLoopInv = 1;

      for (i = 0; i < 9; i++) MatchMx[i] = txrmx->RMx[i];

      for (iNextBasis = 0; iNextBasis < nNextBasis; iNextBasis++)
      {
        if (iNextBasis)
          RotateRotMx(MatchMx, NBRMx, InvNBRMx);

        mmx = MatchMx;

        for (iLoopInv = 0; iLoopInv < nLoopInv; iLoopInv++)
        {
          if (iLoopInv)
          {
            InverseRotMx(MatchMx, InvMatchMx);
            mmx = InvMatchMx;
          }

          for (i = 0; i < 9; i++)
            if (mmx[i] != RMxCopy[i]) break;

          if (i == 9) /* matrix has been found */
          {
            if (RotMxInfo)
            {
              RotMxInfo->Inverse = iLoopInv;

              if (nNextBasis == 3)
              {
                switch(iNextBasis)
                {
                  case 0: RotMxInfo->RefAxis = 'z'; break;
                  case 1: RotMxInfo->RefAxis = 'x'; break;
                  case 2: RotMxInfo->RefAxis = 'y'; break;
                }
              }
              else
                RotMxInfo->RefAxis = 'o';

              RotMxInfo->DirCode = txrmx->DirCode;

              for (i = 0; i < 3; i++)
                RotMxInfo->EigenVector[i] = txrmx->EigenVector[i];

              for (; iNextBasis--;)
              {
                RotMx_t_Vector(REV, NBRMx, RotMxInfo->EigenVector, 0);

                if (iNextBasis-- == 0)
                {
                  for (i = 0; i < 3; i++)
                    RotMxInfo->EigenVector[i] = REV[i];

                  break;
                }

                RotMx_t_Vector(RotMxInfo->EigenVector, NBRMx, REV, 0);
              }
            }

            return Order;
          }
        }
      }

      txrmx++;
    }
  }

  return 0;
}


const T_RotMxInfo *ListOrBufRotMxInfo(const T_SgInfo *SgInfo, int iList,
                                      T_RotMxInfo *BufRotMxInfo)
{
  T_RotMxInfo  *RMxI;


      RMxI = SgInfo->ListRotMxInfo;
  if (RMxI)
      RMxI += iList;
  else
  {
    RMxI = BufRotMxInfo;

    if (GetRotMxInfo(SgInfo->ListSeitzMx[iList].s.R, RMxI) == 0) {
      SetSgError(Err_Ill_SMx_in_List);
      return NULL;
    }
  }

  return RMxI;
}


static int CoreAdd2ListSeitzMx(T_SgInfo *SgInfo, const T_RTMx *NewSMx)
{
  int                  i, iList;
  T_RTMx               *lsmx;
  T_RotMxInfo          RotMxInfo;
  const T_LatticeInfo  *LI;

  static const char *Err_NonXtalOp =
    "Error: Generators produce non-crystallographic operation";


  if (SgInfo->GenOption) LI = SgInfo->LatticeInfo;
  else                   LI = LI_P;

  lsmx = SgInfo->ListSeitzMx;

  for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
    if (CompareSeitzMx(LI, NewSMx, lsmx) == 0)
      return 0; /* matrix is not unique */

  if (GetRotMxInfo(NewSMx->s.R, &RotMxInfo) == 0) {
    SetSgError(Err_NonXtalOp);
    return -1;
  }

  if (SgInfo->nList >= SgInfo->MaxList)
  {
    if (SgInfo->nList >= 192)
      SetSgError(Err_NonXtalOp);
    else
      SetSgError("Internal Error: Allocated space for ListSeitzMx too small");

    return -1;
  }

  for (i = 0; i < 12; i++) lsmx->a[i] = NewSMx->a[i];

  if (SgInfo->ListRotMxInfo != NULL)
    CopyRotMxInfo(&SgInfo->ListRotMxInfo[SgInfo->nList], &RotMxInfo);

  SgInfo->nList++;

  return 1;
}


int Add2ListSeitzMx(T_SgInfo *SgInfo, const T_RTMx *NewSMx)
{
  int     i, special, Enter;
  int     iMult, jMult;
  T_RTMx  TrialSMx, *iMultSMx, *jMultSMx;

  static const char *Err_Ill_lattice_translation =
    "Error: Illegal lattice translation";


  if (SgInfo->nList == 0)
  {
    /* make sure identity matrix is first in list */

    InitSeitzMx(&TrialSMx, 1);

    if (CoreAdd2ListSeitzMx(SgInfo, &TrialSMx) < 0)
      return -1;;
  }

  for (i = 0; i < 9; i++)
        TrialSMx.s.R[i] = NewSMx->s.R[i];

  for (i = 0; i < 3; i++) {
        TrialSMx.s.T[i] = NewSMx->s.T[i] % STBF;
    if (TrialSMx.s.T[i] < 0)
        TrialSMx.s.T[i] += STBF;
  }

  iMult = SgInfo->nList;
  iMultSMx = &SgInfo->ListSeitzMx[iMult];

  jMult = 1;
  jMultSMx = &SgInfo->ListSeitzMx[1]; /* skip first = identity matrix */

  for (;;)
  {
    Enter = 1;

    special = IsSpecialSeitzMx(SgInfo, &TrialSMx, 1);

    if      (special & SpecialSMx_Identity)
    {
      if (! (special & SpecialSMx_Transl0)) {
        SetSgError(Err_Ill_lattice_translation);
        return -1;
      }

      if (SgInfo->GenOption)
        Enter = 0;
    }
    else if (special & SpecialSMx_Inversion)
    {
      if (! (special & SpecialSMx_Transl0))
      {
        if (SgInfo->Centric) {
          SetSgError(Err_Ill_lattice_translation);
          return -1;
        }

        SgInfo->InversionOffOrigin = 1;
      }
      else
      {
        if (SgInfo->InversionOffOrigin)
          SgInfo->Centric = 1;

        SgInfo->InversionOffOrigin = 0;

        if (SgInfo->GenOption)
        {
          if (SgInfo->Centric == 0)
              SgInfo->Centric = -1;

          Enter = 0;
        }
        else
          SgInfo->Centric = 1;
      }
    }

    if (Enter && CoreAdd2ListSeitzMx(SgInfo, &TrialSMx) < 0)
      return -1;

    if (SgInfo->GenOption < 0)
      break;

    if (jMult > iMult)
    {
      iMult++;
      iMultSMx++;

      jMult = 1;
      jMultSMx = &SgInfo->ListSeitzMx[1]; /* skip first = identity matrix */
    }

    if (iMult == SgInfo->nList)
      break;

    SeitzMxMultiply(&TrialSMx, jMultSMx, iMultSMx);

    jMult++;
    jMultSMx++;
  }

  return 0;
}


int AddInversion2ListSeitzMx(T_SgInfo *SgInfo)
{
  T_RTMx  SeitzMx;

  InitSeitzMx(&SeitzMx, -1);
  return Add2ListSeitzMx(SgInfo, &SeitzMx);
}


int AddLatticeTr2ListSeitzMx(T_SgInfo *SgInfo,
                             const T_LatticeInfo *LatticeInfo)
{
  int        iTrV, nTrV;
  const int  *TrV;
  T_RTMx     SeitzMx;


  InitRotMx(SeitzMx.s.R, 1);

  nTrV = LatticeInfo->nTrVector;
   TrV = &LatticeInfo->TrVector[3]; /* skip first vector which is always 000 */

  for (iTrV = 1; iTrV < nTrV; iTrV++)
  {
    SeitzMx.s.T[0] = *TrV++;
    SeitzMx.s.T[1] = *TrV++;
    SeitzMx.s.T[2] = *TrV++;

    if (Add2ListSeitzMx(SgInfo, &SeitzMx) < 0)
      return -1;
  }

  if (SgInfo->GenOption)
    SgInfo->StatusLatticeTr = 0;
  else
    SgInfo->StatusLatticeTr = 1;

  return 0;
}


static int RemoveLatticeTr(T_SgInfo *SgInfo)
{
  int          iList, jList, i;
  T_RTMx       *smxi, *smxj, *lastsmx;
  T_RotMxInfo  *lrmxi, *lastrmxi;


  if (SgInfo->LatticeInfo->Code == 'P')
    return 0;

  if (SgInfo->StatusLatticeTr == -1)
  {
    if (AddLatticeTr2ListSeitzMx(SgInfo, SgInfo->LatticeInfo) < 0)
      return -1;
  }

  iList = 0;

  while (iList < SgInfo->nList)
  {
    smxi = &SgInfo->ListSeitzMx[iList];
    jList = iList + 1;

    while (jList < SgInfo->nList)
    {
      smxj = &SgInfo->ListSeitzMx[jList];

      if (CompareSeitzMx(SgInfo->LatticeInfo, smxi, smxj) == 0)
      {
        /* copy last element to this place */

        SgInfo->nList--;
        lastsmx = &SgInfo->ListSeitzMx[SgInfo->nList];
        for (i = 0; i < 12; i++) smxj->a[i] = lastsmx->a[i];

        if (SgInfo->ListRotMxInfo != NULL)
        {
          lrmxi =    &SgInfo->ListRotMxInfo[jList];
          lastrmxi = &SgInfo->ListRotMxInfo[SgInfo->nList];
          CopyRotMxInfo(lrmxi, lastrmxi);
        }
      }
      else
        jList++;
    }

    iList++;
  }

  SgInfo->StatusLatticeTr = 0;

  return 0;
}


static int RemoveInversion(T_SgInfo *SgInfo)
{
  int          i, iList, special, deter, Centric, InversionOffOrigin;
  T_RTMx       *lsmx, *smx, ProperSMx;
  T_RotMxInfo  *lrmxi, *rmxi;


  Centric = 0;
  InversionOffOrigin = 0;

  lsmx = SgInfo->ListSeitzMx;

  for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
  {
    special = IsSpecialSeitzMx(SgInfo, lsmx, 0);

    if (special & SpecialSMx_Inversion)
    {
      if (special & SpecialSMx_Transl0)
        Centric = 1;
      else
        InversionOffOrigin = 1;

      break;
    }
  }

  if (InversionOffOrigin && Centric) {
    SetSgError("Internal Error: Corrupt SgInfo");
    return -1;
  }

  if (Centric == 0)
  {
    if (InversionOffOrigin) {
      SgInfo->Centric = 0;
      SgInfo->InversionOffOrigin = 1;
    }
    else
    {
      if (SgInfo->Centric) SgInfo->Centric = -1;
      SgInfo->InversionOffOrigin = 0;
    }
  }
  else
  {
    SgInfo->InversionOffOrigin = 0;

    lsmx  = SgInfo->ListSeitzMx;
    lrmxi = SgInfo->ListRotMxInfo;
    iList = 0;

    while (iList < SgInfo->nList)
    {
      deter = deterRotMx(lsmx->s.R);

      if (deter == -1 && SgInfo->Centric == -1)
      {
        for (i = 0; i < 9; i++)
              ProperSMx.s.R[i] = -lsmx->s.R[i];

        for (i = 0; i < 3; i++) {
              ProperSMx.s.T[i] = -lsmx->s.T[i] % STBF;
          if (ProperSMx.s.T[i] < 0)
              ProperSMx.s.T[i] += STBF;
        }

        smx = SgInfo->ListSeitzMx;

        for (i = 0; i < SgInfo->nList; i++, smx++)
          if (CompareSeitzMx(LI_P, &ProperSMx, smx) == 0) break;

        if (i == SgInfo->nList)
        {
          for (i = 0; i < 12; i++) lsmx->a[i] = ProperSMx.a[i];

          deter = deterRotMx(lsmx->s.R);

          if (deter != 1 || (lrmxi && GetRotMxInfo(lsmx->s.R, lrmxi) == 0)) {
            SetSgError(Err_Ill_SMx_in_List);
            return -1;
          }
        }
      }

      if      (deter == -1)
      {
        /* copy last element to this place */

            SgInfo->nList--;
        if (SgInfo->nList != iList)
        {
          smx = &SgInfo->ListSeitzMx[SgInfo->nList];
          for (i = 0; i < 12; i++) lsmx->a[i] = smx->a[i];

          if (lrmxi)
          {
            rmxi = &SgInfo->ListRotMxInfo[SgInfo->nList];
            CopyRotMxInfo(lrmxi, rmxi);
          }
        }
      }
      else if (deter ==  1)
      {
        lsmx++;
        if (lrmxi != NULL) lrmxi++;
        iList++;
      }
      else {
        SetSgError(Err_Ill_SMx_in_List);
        return -1;
      }
    }

    SgInfo->Centric = -1;
  }

  return 0;
}


int ApplyOriginShift(T_SgInfo *SgInfo)
{
  int     HaveOrSh, iList, i;
  T_RTMx  *lsmx, SMx;
  int     OrSh[3], lo[3];


  HaveOrSh = 0;

  for (i = 0; i < 3; i++) {
        OrSh[i] = SgInfo->OriginShift[i] * (STBF / 12);
    if (OrSh[i]) HaveOrSh = 1;
  }

  if (HaveOrSh == 0)
    return 0;

  lsmx = SgInfo->ListSeitzMx;

  for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
  {
    RotMx_t_Vector(lo, lsmx->s.R, OrSh, STBF);

    for (i = 0; i < 3; i++)
      lsmx->s.T[i] = iModPositive(lsmx->s.T[i] - lo[i] + OrSh[i], STBF);
  }

  if (SgInfo->Centric == -1)
  {
    lsmx = &SMx;

    InitSeitzMx(lsmx, -1);

    RotMx_t_Vector(lo, lsmx->s.R, OrSh, STBF);

    for (i = 0; i < 3; i++)
      lsmx->s.T[i] = iModPositive(lsmx->s.T[i] - lo[i] + OrSh[i], STBF);

    if (CoreAdd2ListSeitzMx(SgInfo, lsmx) < 0)
      return -1;

    SgInfo->Centric = 0;
    SgInfo->InversionOffOrigin = 1;
  }

  return 1;
}


static void TidyTranslation(T_SgInfo *SgInfo)
{
  int        iList;
  int        iTrV, nTrV;
  const int  *TrV;
  T_RTMx     *lsmx;
  int        t1, t2, t3, *lt1, *lt2, *lt3, mint1, mint2, mint3;
  int        n0t, n0mint;


  nTrV = SgInfo->LatticeInfo->nTrVector;

  lsmx  = SgInfo->ListSeitzMx;

  for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
  {
    mint1 = *(lt1 = &lsmx->s.T[0]);
    mint2 = *(lt2 = &lsmx->s.T[1]);
    mint3 = *(lt3 = &lsmx->s.T[2]);

    TrV = SgInfo->LatticeInfo->TrVector;

    for (iTrV = 0; iTrV < nTrV; iTrV++)
    {
      t1 = ((*lt1) + *TrV++) % STBF;
      t2 = ((*lt2) + *TrV++) % STBF;
      t3 = ((*lt3) + *TrV++) % STBF;

      n0t = (t1 == 0) + (t2 == 0) + (t3 == 0);
      n0mint = (mint1 == 0) + (mint2 == 0) + (mint3 == 0);

      if (    n0t > n0mint
          || (   n0t == n0mint
              && (   t1 + t2 + t3 <  mint1 + mint2 + mint3
                  || (   t1 + t2 + t3 == mint1 + mint2 + mint3
                      && (t1 < mint1 || (t1 == mint1 && t2 < mint2)))))) {
        mint1 = t1; mint2 = t2; mint3 = t3;
      }
    }

    *lt1 = mint1;
    *lt2 = mint2;
    *lt3 = mint3;
  }
}


static T_SgInfo *Pt_SgInfo_ListSortFunction = NULL;

static int SgInfoListSortFunction(const int *iList_a, const int *iList_b)
{
  int          val_a, val_b, n0_a, n0_b, i;
  T_RTMx       *smx_a, *smx_b;
  T_RotMxInfo  RotMxInfo_a, RotMxInfo_b, *rmxi_a, *rmxi_b;
  T_SgInfo     *SgInfo = Pt_SgInfo_ListSortFunction;


  if (SgError != NULL) return 0;

  if (SgInfo->ListRotMxInfo == NULL)
  {
    rmxi_a = &RotMxInfo_a;
    rmxi_b = &RotMxInfo_b;

    smx_a = &SgInfo->ListSeitzMx[*iList_a];
    smx_b = &SgInfo->ListSeitzMx[*iList_b];

    if (   GetRotMxInfo(smx_a->s.R, rmxi_a) == 0
        || GetRotMxInfo(smx_b->s.R, rmxi_b) == 0)
    {
      SetSgError(Err_Ill_SMx_in_List);
      return 0;
    }
  }
  else
  {
    rmxi_a = &SgInfo->ListRotMxInfo[*iList_a];
    rmxi_b = &SgInfo->ListRotMxInfo[*iList_b];
  }

  val_a = abs(rmxi_a->Order);
  val_b = abs(rmxi_b->Order);

  if (val_a == 1 && val_b != 1) return -1;
  if (val_a != 1 && val_b == 1) return  1;
  if (rmxi_a->Order == 1 && rmxi_b->Order != 1) return -1;
  if (rmxi_a->Order != 1 && rmxi_b->Order == 1) return  1;

  if (val_a != 1)
  {
    if (val_a > val_b) return -1;
    if (val_a < val_b) return  1;
    if (rmxi_a->Order > rmxi_b->Order) return -1;
    if (rmxi_a->Order < rmxi_b->Order) return  1;
  }

  n0_a = n0_b = 0;

  for (i = 0; i < 3; i++)
  {
    if (rmxi_a->EigenVector[i] == 0) n0_a++;
    if (rmxi_b->EigenVector[i] == 0) n0_b++;
  }
  if (n0_a > n0_b) return -1;
  if (n0_a < n0_b) return  1;

  val_a = val_b = 0;

  for (i = 0; i < 3; i++)
  {
    if (val_a < abs(rmxi_a->EigenVector[i]))
        val_a = abs(rmxi_a->EigenVector[i]);
    if (val_b < abs(rmxi_b->EigenVector[i]))
        val_b = abs(rmxi_b->EigenVector[i]);
  }
  if (val_a < val_b) return -1;
  if (val_a > val_b) return  1;

  val_a =  100 * abs(rmxi_a->EigenVector[2]);
  val_a +=  10 * abs(rmxi_a->EigenVector[0]);
  val_a +=       abs(rmxi_a->EigenVector[1]);
  val_b =  100 * abs(rmxi_b->EigenVector[2]);
  val_b +=  10 * abs(rmxi_b->EigenVector[0]);
  val_b +=       abs(rmxi_b->EigenVector[1]);

  if (n0_a < 2)
  {
    if (val_a < val_b) return -1;
    if (val_a > val_b) return  1;
  }
  else
  {
    if (val_a > val_b) return -1;
    if (val_a < val_b) return  1;
  }

  for (i = 0; i < 3; i++)
  {
    if (rmxi_a->EigenVector[i] > rmxi_b->EigenVector[i]) return -1;
    if (rmxi_a->EigenVector[i] < rmxi_b->EigenVector[i]) return  1;
  }

  if (rmxi_a->Inverse < rmxi_b->Inverse) return -1;
  if (rmxi_a->Inverse > rmxi_b->Inverse) return  1;

  smx_a = &SgInfo->ListSeitzMx[*iList_a];
  smx_b = &SgInfo->ListSeitzMx[*iList_b];

  for (i = 0; i < 3; i++)
  {
    if (smx_a->s.T[i] < smx_b->s.T[i]) return -1;
    if (smx_a->s.T[i] > smx_b->s.T[i]) return  1;
  }

  return 0;
}


static int PostSortSgInfoList(const T_SgInfo *SgInfo, int *List_iList)
{
  int                nList, iL_iL, jL_iL;
  T_RTMx             BufMx1, BufMx2, *smxab;
  const T_RTMx       *lsmx, *smxb;
  T_RotMxInfo        RotMxInfo;
  const T_RotMxInfo  *rmxi;
  int                nO_1, iO, save, i, i_;


  nList = SgInfo->nList;

  iL_iL = 0;

  while (iL_iL < nList)
  {
    lsmx = &SgInfo->ListSeitzMx[List_iList[iL_iL]];

        rmxi = ListOrBufRotMxInfo(SgInfo, List_iList[iL_iL], &RotMxInfo);
    if (rmxi == NULL)
      return -1;

    iL_iL++;

    iO = rmxi->Order;
    if (iO < 0 && iO % 2) iO *= 2;
    nO_1 = abs(iO) - 1;

    smxab = &BufMx2;
    smxb = lsmx;

    for (iO = 1; iO < nO_1; iO++)
    {
      SeitzMxMultiply(smxab, lsmx, smxb);

      for (jL_iL = iL_iL; jL_iL < nList; jL_iL++)
      {
        smxb = &SgInfo->ListSeitzMx[List_iList[jL_iL]];
        if (CompareSeitzMx(SgInfo->LatticeInfo, smxab, smxb) == 0)
          break;
      }

      if (jL_iL < nList)
      {
                              save = List_iList[jL_iL];

        for (i = i_ = jL_iL; i > iL_iL; i = i_)
          List_iList[i] = List_iList[--i_];

        List_iList[iL_iL++] = save;
      }

      smxb = smxab;
      if (iO % 2) smxab = &BufMx1;
      else        smxab = &BufMx2;
    }
  }

  return 0;
}


static void SortSgInfoList(T_SgInfo *SgInfo, int *List_iList)
{
  int          i, j, refi;
  int          nList;
  T_RTMx       *lsmx;
  T_RotMxInfo  *lrmxi;


  if (SgError != NULL) return;

  nList = SgInfo->nList;
  lsmx  = SgInfo->ListSeitzMx;
  lrmxi = SgInfo->ListRotMxInfo;
  Pt_SgInfo_ListSortFunction = SgInfo;

  for (i = 0; i < nList; i++) List_iList[i] = i;

  qsort((void *) List_iList, nList, sizeof (*List_iList),
        (int (*)(const void *, const void *)) SgInfoListSortFunction);

  Pt_SgInfo_ListSortFunction = NULL;
  if (SgError != NULL) return;

  if (PostSortSgInfoList(SgInfo, List_iList) != 0)
    return;

  for (i = 0; i < nList; i++)
  {
    j = List_iList[i];

    if (j != i)
    {
      for (refi = i + 1; refi < nList; refi++)
        if (List_iList[refi] == i) break;

      if (refi >= nList) {
        SetSgError("Internal Error: SortSgInfoList(): Corrupt List_iList");
        return;
      }

      SwapRTMx(&lsmx[i], &lsmx[j]);
      if (lrmxi != NULL)
        SwapRotMxInfo(&lrmxi[i], &lrmxi[j]);

      List_iList[refi] = j;
    }
  }
}


int FindSeitzMx(const T_SgInfo *SgInfo,
                int Order, int HonorSign, int RefAxis, int DirCode)
{
  int          iList;
  int          MatchingOrder;
  T_RTMx       *lsmx;
  T_RotMxInfo  *lrmxi, RotMxInfo;


  lsmx  = SgInfo->ListSeitzMx;
  lrmxi = SgInfo->ListRotMxInfo;

  if (lrmxi == NULL) lrmxi = &RotMxInfo;

  for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
  {
    if (lrmxi == &RotMxInfo)
    {
      if (GetRotMxInfo(lsmx->s.R, lrmxi) == 0) {
        SetSgError(Err_Ill_SMx_in_List);
        return -1;
      }
    }

    if (HonorSign == 0)
      MatchingOrder = (abs(Order) == abs(lrmxi->Order));
    else
      MatchingOrder = (Order == lrmxi->Order);

    if (   MatchingOrder
        && lrmxi->Inverse == 0
        && (RefAxis == 0 || RefAxis == lrmxi->RefAxis)
        && (DirCode == 0 || DirCode == lrmxi->DirCode))
    {
      if (DirCode != '*') return iList;

      if (   lrmxi->EigenVector[0] == 1
          && lrmxi->EigenVector[1] == 1
          && lrmxi->EigenVector[2] == 1) return iList;
    }

    if (lrmxi != &RotMxInfo) lrmxi++;
  }

  return -1;
}


static int FindXtalSystem(T_SgInfo *SgInfo)
{
  int                iList, i;
  int                HonorSign = 0, CheckEnantiomorph;
  const T_RTMx       *lsmx;
  T_RotMxInfo        RotMxInfo;
  const T_RotMxInfo  *lrmxi;

  enum Axis { N1 = 0, N2, N3, N4, N6, EndOfAxis };
  int                         N_count[EndOfAxis];


  SgInfo->XtalSystem    = XS_Unknown;
  SgInfo->UniqueRefAxis = 0;
  SgInfo->UniqueDirCode = 0;
  SgInfo->ExtraInfo     = EI_Unknown;

  CheckEnantiomorph = 0;

  for (i = 0; i < EndOfAxis; i++) N_count[i] = 0;

  lsmx  = SgInfo->ListSeitzMx;
  lrmxi = SgInfo->ListRotMxInfo;

  if (lrmxi == NULL) lrmxi = &RotMxInfo;

  for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
  {
    if (lrmxi == &RotMxInfo)
    {
      if (GetRotMxInfo(lsmx->s.R, &RotMxInfo) == 0) {
        SetSgError(Err_Ill_SMx_in_List);
        return XS_Unknown;
      }
    }

    switch(abs(lrmxi->Order))
    {
      case 1:  i = N1; break;
      case 2:  i = N2; break;
      case 3:  i = N3; break;
      case 4:  i = N4; break;
      case 6:  i = N6; break;
      default:
        SetSgError("Internal Error: FindXtalSystem(): Corrupt ListRotMxInfo");
        return XS_Unknown;
    }

    if (lrmxi->Inverse == 0) /* skip N^-1 */
      N_count[i]++;

    if (lrmxi != &RotMxInfo) lrmxi++;
  }

  i = EndOfAxis;

  if (SgInfo->InversionOffOrigin == 1)
  {
    for (i = 0; i < EndOfAxis; i++)
    {
      if (N_count[i] % 2) break;
      N_count[i] /= 2;
    }
  }

  if (i == EndOfAxis)
  {
    if      (N_count[N3] == 4) SgInfo->XtalSystem = XS_Cubic;
    else if (N_count[N3] >  1) SgInfo->XtalSystem = XS_Unknown;
    else if (N_count[N6] == 1) SgInfo->XtalSystem = XS_Hexagonal;
    else if (N_count[N6] >  0) SgInfo->XtalSystem = XS_Unknown;
    else if (N_count[N3] == 1) SgInfo->XtalSystem = XS_Trigonal;
    else if (N_count[N4] == 1) SgInfo->XtalSystem = XS_Tetragonal;
    else if (N_count[N4] >  0) SgInfo->XtalSystem = XS_Unknown;
    else if (N_count[N2] >  2) SgInfo->XtalSystem = XS_Orthorhombic;
    else if (N_count[N2] >  0) SgInfo->XtalSystem = XS_Monoclinic;
    else if (N_count[N1] >  0) SgInfo->XtalSystem = XS_Triclinic;

    HonorSign = 1;
    iList = FindSeitzMx(SgInfo, -1, HonorSign, 'o', '.');
    if (iList < 0) HonorSign = 0;

    switch(SgInfo->XtalSystem)
    {
      case XS_Monoclinic:
        iList = FindSeitzMx(SgInfo, 2, HonorSign, 0, '=');
        if (iList < 0) SgInfo->XtalSystem = XS_Unknown;
        break;
      case XS_Tetragonal:
        CheckEnantiomorph = 1;
        iList = FindSeitzMx(SgInfo, 4, HonorSign, 0, '=');
        if (iList < 0) SgInfo->XtalSystem = XS_Unknown;
        break;
      case XS_Trigonal:
        CheckEnantiomorph = 1;
          iList = FindSeitzMx(SgInfo, 3, HonorSign, 0, '=');
        if (iList < 0)
          iList = FindSeitzMx(SgInfo, 3, HonorSign, 0, '*');
        if (iList < 0) SgInfo->XtalSystem = XS_Unknown;
        break;
      case XS_Hexagonal:
        CheckEnantiomorph = 1;
        iList = FindSeitzMx(SgInfo, 6, HonorSign, 0, '=');
        if (iList < 0) SgInfo->XtalSystem = XS_Unknown;
        break;
      case XS_Cubic:
        iList = FindSeitzMx(SgInfo, 4, HonorSign, 0, '=');
        if (iList >= 0) CheckEnantiomorph = 1;
        break;
      default:
        iList = -1;
        break;
    }
  }

  if (SgInfo->XtalSystem == XS_Unknown) {
    SetSgError("Error: Can't determine crystal system");
  }
  else if (iList >= 0)
  {
        lrmxi = ListOrBufRotMxInfo(SgInfo, iList, &RotMxInfo);
    if (lrmxi == NULL) {
      SgInfo->XtalSystem = XS_Unknown;
      return XS_Unknown;
    }

    if (SgInfo->XtalSystem != XS_Cubic)
    {
      SgInfo->UniqueRefAxis = lrmxi->RefAxis;
      SgInfo->UniqueDirCode = lrmxi->DirCode;

      if (SgInfo->XtalSystem == XS_Trigonal && lrmxi->DirCode == '=')
      {
        switch (lrmxi->RefAxis)
        {
          case 'z':
            switch (SgInfo->LatticeInfo->Code)
            {
              case 'R': SgInfo->ExtraInfo = EI_Obverse; break;
              case 'T': SgInfo->ExtraInfo = EI_Reverse; break;
            }
            break;
          case 'y':
            switch (SgInfo->LatticeInfo->Code)
            {
              case 'S': SgInfo->ExtraInfo = EI_Obverse; break;
              case 'R': SgInfo->ExtraInfo = EI_Reverse; break;
            }
            break;
          case 'x':
            switch (SgInfo->LatticeInfo->Code)
            {
              case 'T': SgInfo->ExtraInfo = EI_Obverse; break;
              case 'S': SgInfo->ExtraInfo = EI_Reverse; break;
            }
            break;
        }
      }
    }

    if (   HonorSign == 0 /* no inversion matrix */
        && SgInfo->LatticeInfo->Code == 'P'
        && CheckEnantiomorph == 1)
    {
      lsmx = &SgInfo->ListSeitzMx[iList];

      if (GetRotMxOrder(lsmx->s.R) > 1)
      {
        i =  lsmx->s.T[0] * lrmxi->EigenVector[0];
        i += lsmx->s.T[1] * lrmxi->EigenVector[1];
        i += lsmx->s.T[2] * lrmxi->EigenVector[2];

        if (i % (STBF / 2)) SgInfo->ExtraInfo = EI_Enantiomorphic;
      }
    }
  }

  return SgInfo->XtalSystem;
}


static int BuildGenerator_iList(T_SgInfo *SgInfo)
{
  int  iList, iList_1, nG;
  int  SgInfo_CI, HonorSign, Flag3asterisk, FlagPG;


  SgInfo_CI = (SgInfo->Centric || SgInfo->InversionOffOrigin);

  SgInfo->PointGroup = PG_Unknown;

        nG   = SgInfo->nGenerator = 0;
#define G_iL   SgInfo->Generator_iList

  HonorSign = 1;
  iList_1 = FindSeitzMx(SgInfo, -1, HonorSign, 'o', '.');
  if (iList_1 < 0) HonorSign = 0;

  if (SgInfo->XtalSystem == XS_Unknown)
    FindXtalSystem(SgInfo);

  switch(SgInfo->XtalSystem)
  {
    case XS_Triclinic:
      if (iList_1 < 0)
        iList_1 = FindSeitzMx(SgInfo, 1, HonorSign, 'o', '.');
      if (iList_1 >= 0) G_iL[nG++] = iList_1;

      if (SgInfo_CI)
        SgInfo->PointGroup = PG_1b;
      else
        SgInfo->PointGroup = PG_1;

      SgInfo->nGenerator = nG;
      return 0;

    case XS_Monoclinic:
      iList = FindSeitzMx(SgInfo, 2, HonorSign, 0, '=');
      if (iList < 0) break;
      G_iL[nG++] = iList;

      if (SgInfo_CI)
        SgInfo->PointGroup = PG_2_m;
      else if (deterRotMx(SgInfo->ListSeitzMx[iList].s.R) == -1)
        SgInfo->PointGroup = PG_m;
      else
        SgInfo->PointGroup = PG_2;

      if (iList_1 >= 0) G_iL[nG++] = iList_1;

      SgInfo->nGenerator = nG;
      return 0;

    case XS_Orthorhombic:
      iList = FindSeitzMx(SgInfo, 2, HonorSign, 'z', '=');
      if (iList >= 0) G_iL[nG++] = iList;

      iList = FindSeitzMx(SgInfo, 2, HonorSign, 'x', '=');
      if (iList >= 0) G_iL[nG++] = iList;

      if (nG < 2)
      {
        iList = FindSeitzMx(SgInfo, 2, HonorSign, 'y', '=');
        if (iList >= 0) G_iL[nG++] = iList;
      }

      if (nG != 2) break;

      if (SgInfo_CI)
        SgInfo->PointGroup = PG_mmm;
      else if (   deterRotMx(SgInfo->ListSeitzMx[G_iL[0]].s.R) == -1
               || deterRotMx(SgInfo->ListSeitzMx[G_iL[1]].s.R) == -1)
        SgInfo->PointGroup = PG_mm2;
      else
        SgInfo->PointGroup = PG_222;

      if (iList_1 >= 0) G_iL[nG++] = iList_1;

      SgInfo->nGenerator = nG;
      return 0;

    case XS_Tetragonal:
      iList = FindSeitzMx(SgInfo, 4, HonorSign, 0, '=');
      if (iList < 0) break;
      G_iL[nG++] = iList;

      if (          SgInfo->UniqueRefAxis != 'x')
      {
        iList = FindSeitzMx(SgInfo, 2, HonorSign, 'x', '=');
        if (iList >= 0) G_iL[nG++] = iList;
      }
      if (nG < 2 && SgInfo->UniqueRefAxis != 'z')
      {
        iList = FindSeitzMx(SgInfo, 2, HonorSign, 'z', '=');
        if (iList >= 0) G_iL[nG++] = iList;
      }
      if (nG < 2 && SgInfo->UniqueRefAxis != 'y')
      {
        iList = FindSeitzMx(SgInfo, 2, HonorSign, 'y', '=');
        if (iList >= 0) G_iL[nG++] = iList;
      }

      if (nG < 2)
      {
        if (SgInfo_CI)
          SgInfo->PointGroup = PG_4_m;
        else if (deterRotMx(SgInfo->ListSeitzMx[G_iL[0]].s.R) == -1)
          SgInfo->PointGroup = PG_4b;
        else
          SgInfo->PointGroup = PG_4;
      }
      else
      {
        if (SgInfo_CI)
          SgInfo->PointGroup = PG_4_mmm;
        else if (deterRotMx(SgInfo->ListSeitzMx[G_iL[0]].s.R) == -1)
        {
          if (deterRotMx(SgInfo->ListSeitzMx[G_iL[1]].s.R) == -1)
            SgInfo->PointGroup = PG_4bm2;
          else
            SgInfo->PointGroup = PG_4b2m;
        }
        else
        {
          if (deterRotMx(SgInfo->ListSeitzMx[G_iL[1]].s.R) == -1)
            SgInfo->PointGroup = PG_4mm;
          else
            SgInfo->PointGroup = PG_422;
        }
      }

      if (iList_1 >= 0) G_iL[nG++] = iList_1;

      SgInfo->nGenerator = nG;
      return 0;

    case XS_Trigonal:
    case XS_Hexagonal:
      Flag3asterisk = 0;

      if (SgInfo->XtalSystem == XS_Trigonal)
      {
          iList = FindSeitzMx(SgInfo, 3, HonorSign, 0, '=');
        if (iList < 0)
        {
          iList = FindSeitzMx(SgInfo, 3, HonorSign, 0, '*');
          Flag3asterisk = 1;
        }
      }
      else
        iList = FindSeitzMx(SgInfo, 6, HonorSign, 0, '=');

      if (iList < 0) break;
      G_iL[nG++] = iList;

      iList = FindSeitzMx(SgInfo, 2, HonorSign, 0, '\'');
      if (iList >= 0) G_iL[nG++] = iList;

      FlagPG = -1;

      if (nG < 2)
      {
        iList = FindSeitzMx(SgInfo, 2, HonorSign, 0, '"');
        if (iList >= 0) G_iL[nG++] = iList;
        FlagPG = 1;
      }

      if (SgInfo->XtalSystem == XS_Trigonal)
      {
        if (nG < 2)
        {
          if (SgInfo_CI) SgInfo->PointGroup = PG_3b;
          else           SgInfo->PointGroup = PG_3;
        }
        else
        {
          if (Flag3asterisk == 1)
          {
            if (SgInfo_CI)
              SgInfo->PointGroup = PG_3bm;
            else
            {     FlagPG = deterRotMx(SgInfo->ListSeitzMx[G_iL[1]].s.R);
              if (FlagPG == -1) SgInfo->PointGroup = PG_3m;
              else              SgInfo->PointGroup = PG_32;
            }
          }
          else if (FlagPG == -1)
          {
            if (SgInfo_CI)
              SgInfo->PointGroup = PG_3b1m;
            else
            {     FlagPG = deterRotMx(SgInfo->ListSeitzMx[G_iL[1]].s.R);
              if (FlagPG == -1) SgInfo->PointGroup = PG_31m;
              else              SgInfo->PointGroup = PG_312;
            }
          }
          else
          {
            if (SgInfo_CI)
              SgInfo->PointGroup = PG_3bm1;
            else
            {     FlagPG = deterRotMx(SgInfo->ListSeitzMx[G_iL[1]].s.R);
              if (FlagPG == -1) SgInfo->PointGroup = PG_3m1;
              else              SgInfo->PointGroup = PG_321;
            }
          }
        }
      }
      else
      {
        if (nG < 2)
        {
          if (SgInfo_CI)
            SgInfo->PointGroup = PG_6_m;
          else if (deterRotMx(SgInfo->ListSeitzMx[G_iL[0]].s.R) == -1)
            SgInfo->PointGroup = PG_6b;
          else
            SgInfo->PointGroup = PG_6;
        }
        else
        {
          if (SgInfo_CI)
            SgInfo->PointGroup = PG_6_mmm;
          else if (deterRotMx(SgInfo->ListSeitzMx[G_iL[0]].s.R) == -1)
          {
            if (deterRotMx(SgInfo->ListSeitzMx[G_iL[1]].s.R) == FlagPG)
              SgInfo->PointGroup = PG_6b2m;
            else
              SgInfo->PointGroup = PG_6bm2;
          }
          else if (deterRotMx(SgInfo->ListSeitzMx[G_iL[1]].s.R) == -1)
            SgInfo->PointGroup = PG_6mm;
          else
            SgInfo->PointGroup = PG_622;
        }
      }

      if (iList_1 >= 0) G_iL[nG++] = iList_1;

      SgInfo->nGenerator = nG;
      return 0;

    case XS_Cubic:
      FlagPG = 0;

        iList = FindSeitzMx(SgInfo, 4, HonorSign, 'z', '=');
      if (iList < 0) {
        iList = FindSeitzMx(SgInfo, 2, HonorSign, 'z', '=');
        FlagPG = 1;
      }
      if (iList < 0) break;
      G_iL[nG++] = iList;

      iList = FindSeitzMx(SgInfo, 2, HonorSign, 'x', '=');
      if (iList < 0) break;
      G_iL[nG++] = iList;

      iList = FindSeitzMx(SgInfo, 3, HonorSign, 'o', '*');
      if (iList < 0) break;
      G_iL[nG++] = iList;

      if (FlagPG)
      {
        if (SgInfo_CI) SgInfo->PointGroup = PG_m3b;
        else           SgInfo->PointGroup = PG_23;
      }
      else
      {
        if (SgInfo_CI)
          SgInfo->PointGroup = PG_m3bm;
        else if (deterRotMx(SgInfo->ListSeitzMx[G_iL[0]].s.R) == -1)
          SgInfo->PointGroup = PG_4b3m;
        else
          SgInfo->PointGroup = PG_432;
      }

      if (iList_1 >= 0) G_iL[nG++] = iList_1;

      SgInfo->nGenerator = nG;
      return 0;

    default:
      break;
  }

#undef G_iL

  return -1;
}


static int BuildHSym(T_SgInfo *SgInfo)
{
  int                NeedDash, HaveOrSh, nGRT, iList, iG, ip, os, i;
  int                AbsOrder, RefAxis, DirCode, ScrewPrimitive, Screw;
  int                PreviousRotation, PreviousRefAxis, PreviousDirCode;
  int                nTrV, iTrV, OrSh[3], RO[3], Transl[3];
  const int          *TrV, *ht;
  T_RTMx             SMx_1;
  const T_RTMx       *SMx;
  const T_RotMxInfo  *rmxi;
  char               *hsym, *hsym_mark;

  struct
  {
    T_RotMxInfo         RMxI_Buf;
    const T_RotMxInfo  *RMxI;
    int                Transl[3];
  }
  GRT[sizeof SgInfo->Generator_iList / sizeof (*SgInfo->Generator_iList) + 1];

  const char *Digits = "0123456";


  if (SgInfo->nGenerator == 0) {
    SetSgError("Internal Error: BuildHSym(): Empty generator list");
    return -1;
  }

  HaveOrSh = 0;

  for (i = 0; i < 3; i++) {
        OrSh[i] = SgInfo->OriginShift[i] * (STBF / 12);
    if (OrSh[i]) HaveOrSh = 1;
  }

  NeedDash = 0;
  nGRT = 0;

  for (iG = 0; iG < SgInfo->nGenerator; iG++)
  {
    iList = SgInfo->Generator_iList[iG];

        GRT[nGRT].RMxI = ListOrBufRotMxInfo(SgInfo, iList, &GRT[nGRT].RMxI_Buf);
    if (GRT[nGRT].RMxI == NULL)
      return -1;

    SMx = &SgInfo->ListSeitzMx[iList];

    RotMx_t_Vector(RO, SMx->s.R, OrSh, STBF);

    for (i = 0; i < 3; i++)
      GRT[nGRT].Transl[i] = iModPositive(SMx->s.T[i] + RO[i] - OrSh[i], STBF);

    if (GRT[nGRT].RMxI->Order == -1)
    {
      for (i = 0; i < 3; i++)
        if (GRT[nGRT].Transl[i] != 0) break;

      if (i == 3) NeedDash = 1;
      else        nGRT++;
    }
    else
      nGRT++;
  }

  if (SgInfo->Centric)
  {
    if (HaveOrSh == 0)
      NeedDash = 1;
    else
    {
      for (iG = 0; iG < nGRT; iG++)
        if (GRT[iG].RMxI->Order == 1) break;

      InitSeitzMx(&SMx_1, -1);

      if (GetRotMxInfo(SMx_1.s.R, &GRT[iG].RMxI_Buf) != -1) {
        SetSgError("Internal Error: BuildHSym(): Corrupt GetRotMxInfo()");
        return -1;
      }

      GRT[iG].RMxI = &GRT[iG].RMxI_Buf;

      for (i = 0; i < 3; i++)
        GRT[iG].Transl[i] = iModPositive(-2 * OrSh[i], STBF);

      if (iG == nGRT)
        nGRT++;
    }
  }

  hsym = SgInfo->HallSymbol;

  for (i = 0; i <= MaxLenHallSymbol; i++)
    *hsym++ = '\0';

  PreviousRotation = 0;
  PreviousRefAxis = 0;
  PreviousDirCode = 0;

  hsym = SgInfo->HallSymbol;

  if (NeedDash)
    *hsym++ = '-';
  else
    *hsym++ = ' ';

  *hsym++ = SgInfo->LatticeInfo->Code;

  nTrV = SgInfo->LatticeInfo->nTrVector;

  for (iG = 0; iG < nGRT; iG++)
  {
    rmxi = GRT[iG].RMxI;

    AbsOrder = abs(rmxi->Order);
    RefAxis = rmxi->RefAxis;
    DirCode = rmxi->DirCode;
    if (RefAxis == 'o') RefAxis = 0;
    if (DirCode == '=' || DirCode == '.') DirCode = 0;

    if (iG == 0)
    {
      if (RefAxis == 'z') RefAxis = 0;
    }
    else
    {
      if      (AbsOrder == 2)
      {
        if      (PreviousRotation == 2 || PreviousRotation == 4)
        {
          if (RefAxis == 'x') RefAxis = 0;
        }
        else if (PreviousRotation == 3 || PreviousRotation == 6)
        {
          if (   PreviousDirCode == '*'
              || RefAxis == PreviousRefAxis) RefAxis = 0;
          if (DirCode == '\'') DirCode = 0;
        }
      }
      else if (AbsOrder == 3)
      {
        if (DirCode == '*') DirCode = 0;
      }
    }

    PreviousRotation = AbsOrder;
    PreviousRefAxis = rmxi->RefAxis;
    PreviousDirCode = rmxi->DirCode;

    *hsym++ = ' ';
    if (rmxi->Order < 0) *hsym++ = '-';
    *hsym++ = Digits[AbsOrder];
    if (RefAxis) *hsym++ = RefAxis;
    if (DirCode) *hsym++ = DirCode;

    TrV = SgInfo->LatticeInfo->TrVector;

    for (iTrV = 0; iTrV < nTrV; iTrV++, TrV += 3)
    {
      for (i = 0; i < 3; i++)
        if ((GRT[iG].Transl[i] + TrV[i]) % STBF != 0)
          break;

      if (i == 3)
        break;
    }

    if (iTrV < nTrV)
      continue; /* next iG */

    hsym_mark = hsym;

    TrV = SgInfo->LatticeInfo->TrVector;

    for (iTrV = 0; iTrV < nTrV; iTrV++, TrV += 3, hsym = hsym_mark)
    {
      for (i = 0; i < 3; i++)
        Transl[i] = iModPositive(GRT[iG].Transl[i] + TrV[i], STBF);

      Screw = 0;

      for (ip = 0; ip < 3; ip++)
        if (rmxi->EigenVector[ip] != 0) break;

      if (ip < 3 && rmxi->EigenVector[ip] == 1)
      {
        for (i = ip + 1; i < 3; i++)
          if (rmxi->EigenVector[i] != 0) break;

        if (i == 3)
        {
          ScrewPrimitive = STBF / AbsOrder;
          Screw = Transl[ip] / ScrewPrimitive;
              i = Screw * ScrewPrimitive;
          if (i % 3)
          {
            *hsym++ = Digits[Screw];
            Transl[ip] -= i;
          }
        }
      }

      ht = HallTranslations;

      while (*ht)
      {
        for (i = 0; i < 3; i++)
          if (Transl[i] < ht[i + 1]) break;

        if (i == 3)
        {
          for (i = 0; i < 3; i++)
            Transl[i] -= ht[i + 1];

          *hsym++ = (char) *ht;
        }

        ht += 4;
      }

      for (i = 0; i < 3; i++)
        if (Transl[i] != 0)
          break;

      if (i == 3)
        break;
    }

    if (iTrV == nTrV)
      return 0;
  }

  if (nGRT == 0)
  {
    *hsym++ = ' ';
    *hsym++ = '1';
  }

  if (HaveOrSh)
  {
    *hsym++ = ' ';
    *hsym++ = '(';

    for (i = 0; i < 3; i++)
    {
      if (i) *hsym++ = ' ';

          os = iModPositive(SgInfo->OriginShift[i], 12);
      if (os > 6)
      {
        *hsym++ = '-';
        os = 12 - os;
      }

      *hsym++ = Digits[os];
    }

    *hsym++ = ')';
  }

  *hsym = '\0';

  if (SgInfo->HallSymbol[MaxLenHallSymbol] != '\0') {
    SetSgError("Internal Error: BuildHSym(): MaxLenHallSymbol too small");
    return -1;
  }

  return 1;
}


static int BuildHallSymbol(T_SgInfo *SgInfo, int FixedOriginShift)
{
  int     ix, iy, iz;
  int     status;

  static const int ShiftTable[] = { 0, 1, -1, 2, -2, 3 };


  if (SgError != NULL) return -1;

  if (SgInfo->nGenerator == 0)
  {
    if (BuildGenerator_iList(SgInfo) != 0)
    {
      SetSgError("Error: Can't build generator list");
      return -1;
    }
  }

  if (FixedOriginShift)
  {
        status = BuildHSym(SgInfo);
    if (status == 1)
      return 0;
  }
  else
  {
    for (ix = 0; ix < 6; ix++)
    {
      SgInfo->OriginShift[0] = ShiftTable[ix];

      for (iy = 0; iy < 6; iy++)
      {
        SgInfo->OriginShift[1] = ShiftTable[iy];

        for (iz = 0; iz < 6; iz++)
        {
          SgInfo->OriginShift[2] = ShiftTable[iz];

              status = BuildHSym(SgInfo);
          if (status < 0)
            return -1;

          if (status == 1)
            return 0;
        }
      }
    }
  }

  SetSgError("Error: Can't build Hall Symbol");
  return -1;
}


void InitSgInfo(T_SgInfo *SgInfo)
{
  int  i;


  SgInfo->GenOption = 0;
  SgInfo->Centric = 0;
  SgInfo->InversionOffOrigin = 0;
  SgInfo->LatticeInfo = LI_P;
  SgInfo->StatusLatticeTr = 0;
  for (i = 0; i < 3; i++)
    SgInfo->OriginShift[i] = 0;
  SgInfo->nList = 0;

  SgInfo->OrderL = 0;
  SgInfo->OrderP = 0;
  SgInfo->XtalSystem = XS_Unknown;
  SgInfo->UniqueRefAxis = 0;
  SgInfo->UniqueDirCode = 0;
  SgInfo->ExtraInfo = EI_Unknown;
  SgInfo->PointGroup = PG_Unknown;
  SgInfo->nGenerator = 0;
  SgInfo->HallSymbol[0] = '\0';
  SgInfo->TabSgName = NULL;
  SgInfo->CCMx_LP = NULL;
  SgInfo->n_si_Vector = -1;
}


int CompleteSgInfo(T_SgInfo *SgInfo)
{
  int                List_iList[192];
  const T_TabSgName  *tsgn;


  if (SgInfo->StatusLatticeTr == -1)
  {
    if (AddLatticeTr2ListSeitzMx(SgInfo, SgInfo->LatticeInfo) < 0)
      return -1;
  }

  if (ApplyOriginShift(SgInfo) < 0)
    return -1;

  if (SgInfo->nList >  ((int)sizeof List_iList) / ((int)sizeof(*List_iList))) {
    SetSgError("Internal Error: CompleteSgInfo()");
    return -1;
  }

  if (SgInfo->nList > 1)
  {
    SortSgInfoList(SgInfo, List_iList);
    if (SgError != NULL) return -1;
  }

  if (RemoveLatticeTr(SgInfo) != 0)
    return -1;

  if (RemoveInversion(SgInfo) != 0)
    return -1;

  TidyTranslation(SgInfo);

  if (SgInfo->nList > 1)
  {
    SortSgInfoList(SgInfo, List_iList);
    if (SgError != NULL) return -1;
  }
                             SgInfo->OrderP = SgInfo->nList;
  if (SgInfo->Centric == -1) SgInfo->OrderP *= 2;

  SgInfo->OrderL = SgInfo->OrderP * SgInfo->LatticeInfo->nTrVector;

  if (BuildHallSymbol(SgInfo, 0) != 0)
    return -1;

  for (tsgn = TabSgName; tsgn->HallSymbol; tsgn++)
    if (   strcmp(tsgn->HallSymbol, SgInfo->HallSymbol) == 0
        && (   SgInfo->TabSgName == NULL
            || SgInfo->TabSgName == tsgn))
      break;

  if (SgInfo->TabSgName != NULL && tsgn->HallSymbol == NULL)
  {
    if (SgError) return -1;

    sprintf(SgErrorBuffer,
      "Internal Error: Input/Output HallSymbol mismatch: %s <> %s",
      SgInfo->TabSgName->HallSymbol, SgInfo->HallSymbol);

    SetSgError(SgErrorBuffer);
    return -1;
  }

  if (tsgn->HallSymbol)
    SgInfo->TabSgName = tsgn;

  SgInfo->CCMx_LP = NULL;

  switch (SgInfo->LatticeInfo->Code)
  {
    case 'P': SgInfo->CCMx_LP = CCMx_PP; break;
    case 'A': SgInfo->CCMx_LP = CCMx_AP; break;
    case 'B': SgInfo->CCMx_LP = CCMx_BP; break;
    case 'C': SgInfo->CCMx_LP = CCMx_CP; break;
    case 'I': SgInfo->CCMx_LP = CCMx_IP; break;
    case 'R':
      switch (SgInfo->UniqueRefAxis) {
        case   0:
        case 'z': SgInfo->CCMx_LP = CCMx_RP_z; break;
        case 'y': SgInfo->CCMx_LP = CCMx_RP_y; break;
        default: break;
      }
      break;
    case 'S':
      switch (SgInfo->UniqueRefAxis) {
        case   0:
        case 'y': SgInfo->CCMx_LP = CCMx_SP_y; break;
        case 'x': SgInfo->CCMx_LP = CCMx_SP_x; break;
        default: break;
      }
      break;
    case 'T':
      switch (SgInfo->UniqueRefAxis) {
        case   0:
        case 'x': SgInfo->CCMx_LP = CCMx_TP_x; break;
        case 'z': SgInfo->CCMx_LP = CCMx_TP_z; break;
        default: break;
      }
      break;
    case 'F': SgInfo->CCMx_LP = CCMx_FP; break;
    default:
      break;
  }

  if (SgInfo->CCMx_LP == NULL) {
    SetSgError("Internal Error: Illegal lattice code");
    return -1;
  }

  return 0;
}


int CB_SMx(T_RTMx *CSiC,
           const T_RTMx *CBMx, const T_RTMx *SMx, const T_RTMx *InvCBMx)
{
  int     i;
  T_RTMx  BufMx;


  RTMxMultiply(&BufMx, SMx,  InvCBMx, CTBF / STBF, CTBF);
  RTMxMultiply(CSiC,   CBMx, &BufMx,  CRBF,        CRBF * CTBF);

  for (i = 0; i < 9; i++)
  {
    if (CSiC->s.R[i] % (CRBF * CRBF)) {
      SetSgError("Internal Error: Corrupt CBMx/SMx/InvCBMx");
      return -1;
    }

    CSiC->s.R[i] /= (CRBF * CRBF);
  }

  for (i = 0; i < 3; i++)
  {
    if (CSiC->s.T[i] % (CRBF * (CTBF / STBF))) {
      SetSgError("Internal Error: Out of STBF range");
      return -1;
    }

    CSiC->s.T[i] /= (CRBF * (CTBF / STBF));
  }

  return 0;
}


int TransformSgInfo(const T_SgInfo *SgInfo,
                    const T_RTMx *CBMx, const T_RTMx *InvCBMx,
                    T_SgInfo *BC_SgInfo)
{
  int           iList, f, i;
  int           nTrV, iTrV, nLoopInv, iLoopInv;
  const int     *TrV;
  T_RTMx        SMx, BC_SMx;
  const T_RTMx  *lsmx;


  nLoopInv = Sg_nLoopInv(SgInfo);

  nTrV = SgInfo->LatticeInfo->nTrVector;
   TrV = SgInfo->LatticeInfo->TrVector;

  for (iTrV = 0; iTrV < nTrV; iTrV++, TrV += 3)
  {
    for (iLoopInv = 0; iLoopInv < nLoopInv; iLoopInv++)
    {
      if (iLoopInv == 0) f =  1;
      else               f = -1;

      lsmx = SgInfo->ListSeitzMx;

      for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
      {
        for (i = 0; i < 9; i++)
          SMx.s.R[i] = f * lsmx->s.R[i];

        for (i = 0; i < 3; i++)
          SMx.s.T[i] = f * lsmx->s.T[i] + TrV[i];

        if (CB_SMx(&BC_SMx, CBMx, &SMx, InvCBMx) != 0)
          return -1;

        if (Add2ListSeitzMx(BC_SgInfo, &BC_SMx) < 0)
          return -1;
      }
    }
  }

  return 0;
}


#undef SGCLIB_C__
