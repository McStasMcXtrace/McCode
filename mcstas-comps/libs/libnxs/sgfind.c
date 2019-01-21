/*
  Space Group Info's (c) 1994-96 Ralf W. Grosse-Kunstleve
 */

#include <stdio.h>
#include <stdlib.h>


#define SGCOREDEF__
#include "sginfo.h"


static int InitialCBMxR(T_SgInfo *SgInfo,
                        const T_LatticeInfo **NewLatticeInfo,
                        int *NewPointGroup,
                        int *IniCBMxR, int *IniInvCBMxR)
{
  int                  Code, NewPG, deterCCMx, fac, i;
  const T_LatticeInfo  *NewLI;
  const int            *CCMx;


  Code  = SgInfo->LatticeInfo->Code;
  NewLI = SgInfo->LatticeInfo;
  NewPG = SgInfo->PointGroup;
  CCMx  = CCMx_PP;

  switch(SgInfo->XtalSystem)
  {
    case XS_Triclinic:
      NewLI = LI_P;
      CCMx  = SgInfo->CCMx_LP;
      break;

    case XS_Monoclinic:
    case XS_Tetragonal:
      switch (SgInfo->UniqueRefAxis)
      {
        case 'z': if      (Code == 'C') {
                    NewLI = LI_P; CCMx = SgInfo->CCMx_LP; }
                  else if (Code == 'F') {
                    NewLI = LI_I; CCMx = CCMx_FI_z; }
                  break;
        case 'y': if      (Code == 'B') {
                    NewLI = LI_P; CCMx = SgInfo->CCMx_LP; }
                  else if (Code == 'F') {
                    NewLI = LI_I; CCMx = CCMx_FI_y; }
                  break;
        case 'x': if      (Code == 'A') {
                    NewLI = LI_P; CCMx = SgInfo->CCMx_LP; }
                  else if (Code == 'F') {
                    NewLI = LI_I; CCMx = CCMx_FI_x; }
                  break;
        default:
          goto ReturnError;
      }

      if (   SgInfo->XtalSystem == XS_Tetragonal
          && SgInfo->LatticeInfo != NewLI)
      {
        if      (NewPG == PG_4b2m) NewPG = PG_4bm2;
        else if (NewPG == PG_4bm2) NewPG = PG_4b2m;
      }

      break;

    case XS_Orthorhombic:
      break;

    case XS_Trigonal:
      NewLI = LI_P;
      CCMx  = SgInfo->CCMx_LP;

      if (Code == 'R' || Code == 'S' || Code == 'T')
      {
        if      (NewPG == PG_321)  NewPG = PG_32;
        else if (NewPG == PG_3m1)  NewPG = PG_3m;
        else if (NewPG == PG_3bm1) NewPG = PG_3bm;
      }

      break;

    case XS_Hexagonal:
      break;

    case XS_Cubic:
      break;

    default:
      goto ReturnError;
  }

      deterCCMx = deterRotMx(CCMx);
  if (deterCCMx < 1 || CRBF % deterCCMx)
    goto ReturnError;

  fac = CRBF / deterCCMx;

  InverseRotMx(CCMx, IniInvCBMxR);

  for (i = 0; i < 9; i++) {
       IniCBMxR[i] = CRBF * CCMx[i];
    IniInvCBMxR[i] *= fac;
  }

  *NewLatticeInfo = NewLI;
  *NewPointGroup  = NewPG;

  return deterCCMx;

  ReturnError:

  SetSgError("Internal Error: InitialCBMxR()");
  return -1;
}


static int CBR_RMx(int *RRMx,
                   const int *CBMxR, const int *RMx, const int *InvCBMxR)
{
  int  i, BufMx[9];


  RotMxMultiply(BufMx, RMx,   InvCBMxR);
  RotMxMultiply(RRMx,  CBMxR, BufMx);

  for (i = 0; i < 9; i++)
  {
    if (RRMx[i] % (CRBF * CRBF)) {
      SetSgError("Internal Error: CBR_SMx()");
      return -1;
    }

    RRMx[i] /= (CRBF * CRBF);
  }

  return 0;
}


static void RotateCBMxR(const int *RMx, const int *InvRMx,
                        int *CBMxR, int *InvCBMxR)
{
  int  i, BufMx[9];


  RotMxMultiply(BufMx, RMx, CBMxR);
  for (i = 0; i < 9; i++)    CBMxR[i] = BufMx[i];

  /* matrix algebra: (A * B)^-1 = B^-1 * A^-1 */

  RotMxMultiply(BufMx, InvCBMxR, InvRMx);
  for (i = 0; i < 9; i++) InvCBMxR[i] = BufMx[i];
}


static int AlignUniqueAxis(const T_SgInfo *SgInfo,
                           const T_SgInfo *GenSgI,
                           int *CBMxR, int *InvCBMxR,
                           const int **AlignRMx)
{
  int          i, iListS, DirCode;
  int          UAMx[9], RotEV[3];
  const int    *RMx, *InvRMx, *lsmxR;
  T_RotMxInfo  RMxI_S, *RMxI_G;


  if (GenSgI->nList < 2)
    goto ReturnError;

  RMxI_G = &GenSgI->ListRotMxInfo[1];

  if (abs(RMxI_G->Order) == 3) DirCode = 0;
  else                         DirCode = '=';

      iListS =   FindSeitzMx(SgInfo,  RMxI_G->Order, 1, 0, DirCode);
  if (iListS < 0)
  {
    if (SgInfo->Centric == 0)
      return 0;

        iListS = FindSeitzMx(SgInfo, -RMxI_G->Order, 1, 0, DirCode);
    if (iListS < 0)
      goto ReturnError;

    for (i = 0; i < 9; i++)
      UAMx[i] = -SgInfo->ListSeitzMx[iListS].s.R[i];

    lsmxR = UAMx;
  }
  else
    lsmxR = SgInfo->ListSeitzMx[iListS].s.R;

  if (CBR_RMx(UAMx, CBMxR, lsmxR, InvCBMxR) != 0)
    goto ReturnError;

  if (GetRotMxInfo(UAMx, &RMxI_S) != RMxI_G->Order)
    goto ReturnError;

  if (RMxI_S.DirCode != RMxI_G->DirCode)
    return 0;

  RMx = InvRMx = RMx_1_000;

  for (;;)
  {
    RotMx_t_Vector(RotEV, RMx, RMxI_S.EigenVector, 0);

    for (i = 0; i < 3; i++)
      if (RotEV[i] != RMxI_G->EigenVector[i]) break;

    if (i == 3)
      break;

    if      (RMxI_S.DirCode == '=')
    {
      if      (RMx == RMx_1_000) {
               RMx =  RMx_3_111; InvRMx = RMx_3i111; }
      else if (RMx == RMx_3_111) {
               RMx =  RMx_3i111; InvRMx = RMx_3_111; }
      else
        goto ReturnError;
    }
    else if (RMxI_S.DirCode == '*')
    {
      if      (RMx == RMx_1_000) {
               RMx =  RMx_4_001; InvRMx = RMx_4i001; }
      else if (RMx == RMx_4_001) {
               RMx =  RMx_2_001; InvRMx = RMx_2_001; }
      else if (RMx == RMx_2_001) {
               RMx =  RMx_4i001; InvRMx = RMx_4_001; }
      else
        goto ReturnError;
    }
    else
      goto ReturnError;
  }

  if (RMx != RMx_1_000)
    RotateCBMxR(RMx, InvRMx, CBMxR, InvCBMxR);

  if (AlignRMx)
     *AlignRMx = RMx;

  return 1;

  ReturnError:

  SetSgError("Internal Error: AlignUniqueAxis()");
  return -1;
}


static const T_RTMx *GetSMxWithSameRot(const int *WtdRotMx,
                                       const T_SgInfo *SgInfo, T_RTMx *BufMx)
{
  int           iList, i;
  const T_RTMx  *lsmx;


  lsmx = SgInfo->ListSeitzMx;

  for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
  {
    for (i = 0; i < 9; i++)
      if (WtdRotMx[i] !=  lsmx->s.R[i])
        break;

    if (i == 9)
      return lsmx;

    if (SgInfo->Centric != -1)
      continue;

    for (i = 0; i < 9; i++)
      if (WtdRotMx[i] != -lsmx->s.R[i])
        break;

    if (i == 9)
    {
      for (i = 0; i < 9; i++)
            BufMx->s.R[i] = -lsmx->s.R[i];

      for (i = 0; i < 3; i++) {
            BufMx->s.T[i] = -lsmx->s.T[i] % STBF;
        if (BufMx->s.T[i] < 0)
            BufMx->s.T[i] += STBF;
      }

      return BufMx;
    }
  }

  return NULL;
}


static int BuildFreeMx(const int *EigenVector, int Order, int *FreeMx)
{
  static const int GeneratorEigenVectors[] =
    {
       001,   0,  0,  1,
       010,   0,  1,  0,
       100,   1,  0,  0,
       110,   1,  1,  0,
      -110,   1, -1,  0,
       111,   1,  1,  1,
         0
    };

  int        i;
  const int  *gev;


  for (i = 0; i < 9; i++)
    FreeMx[i] = 0;

  if (Order == -1 || Order == -3 || Order == -4 || Order == -6)
    return 0;

  for (gev = GeneratorEigenVectors; *gev++ != 0; gev += 3)
  {
    for (i = 0; i < 3; i++)
      if (EigenVector[i] != gev[i])
        break;

    if (i == 3)
      break;
  }

  gev--;

  if      (Order == -2)
  {
    switch (*gev)
    {
      case  001: FreeMx[0] =  1; FreeMx[4] =  1;                 return 0;
      case  010: FreeMx[8] =  1; FreeMx[0] =  1;                 return 0;
      case  100: FreeMx[4] =  1; FreeMx[8] =  1;                 return 0;
      case  110: FreeMx[1] =  1; FreeMx[4] = -1; FreeMx[8] =  1; return 1;
      case -110: FreeMx[1] =  1; FreeMx[4] =  1; FreeMx[8] =  1; return 1;
      default:
        break;
    }
  }
  else if (Order > 1)
  {
    switch (*gev)
    {
      case  001: FreeMx[8] =  1;                                 return 0;
      case  010: FreeMx[4] =  1;                                 return 0;
      case  100: FreeMx[0] =  1;                                 return 0;
      case  110: FreeMx[0] =  1; FreeMx[3] =  1;                 return 1;
      case -110: FreeMx[0] =  1; FreeMx[3] = -1;                 return 1;
      case  111: FreeMx[0] =  1; FreeMx[3] =  1; FreeMx[6] =  1; return 1;
      default:
        break;
    }
  }

  SetSgError("Internal Error: BuildFreeMx()");
  return -1;
}


static int StartFixAxes(const T_SgInfo *SgInfo,
                        const T_SgInfo *GenSgI, const int *iGen,
                        T_RTMx *CBMx, T_RTMx *InvCBMx,
                        T_RTMx *SMxG, T_RTMx *SMxS_G,
                        int *FreeMx, int *TryAgain)
{
  int                iG, Order, i;
  const int          *EV;
  T_RTMx             SMxG_S, BufMx;
  const T_RTMx       *SMx;
  const T_RotMxInfo  *RMxI_G;


  if (*iGen == -3)
    iG = 1;
  else
    iG = *iGen;

  if (iG == -1)
  {
    Order = -1;
    EV    = NULL;
  }
  else
  {
    if (iG < 1 || iG >= GenSgI->nList)
      goto ReturnError;

            RMxI_G = &GenSgI->ListRotMxInfo[iG];
    Order = RMxI_G->Order;
    EV    = RMxI_G->EigenVector;

    if (iG != *iGen)
    {
          Order *= -1;
      if (Order != *iGen)
        goto ReturnError;
    }
  }

  if (Order == -1)
  {
    if (GenSgI->Centric == -1) {
      InitSeitzMx(SMxG, -1);
    }
    else
    {
      for (iG = 1; iG < GenSgI->nList; iG++)
        if (GenSgI->ListRotMxInfo[iG].Order == -1)
          break;

      if (iG == GenSgI->nList)
        goto ReturnError;

      SMx = &GenSgI->ListSeitzMx[iG];

      for (i = 0; i < 12; i++) SMxG->a[i] = SMx->a[i];
    }
  }
  else
  {
    SMx = &GenSgI->ListSeitzMx[iG];

    if (iG == *iGen)
      for (i = 0; i < 12; i++) SMxG->a[i] = SMx->a[i];
    else
    {
      for (i = 0; i < 9; i++)
            SMxG->s.R[i] = -SMx->s.R[i];

      for (i = 0; i < 3; i++) {
            SMxG->s.T[i] = -SMx->s.T[i] % STBF;
        if (SMxG->s.T[i] < 0)
            SMxG->s.T[i] += STBF;
      }
    }
  }

  if (CB_SMx(&SMxG_S, InvCBMx, SMxG, CBMx) != 0)
    return -1;

      SMx = GetSMxWithSameRot(SMxG_S.s.R, SgInfo, &BufMx);
  if (SMx == NULL)
    return 0;

  if (CB_SMx(SMxS_G, CBMx, SMx, InvCBMx) != 0)
    return -1;

  for (i = 0; i < 9; i++)
    if (SMxS_G->s.R[i] != SMxG->s.R[i])
      goto ReturnError;

      *TryAgain = BuildFreeMx(EV, Order, FreeMx);
  if (*TryAgain < 0)
    return -1;

  return 1;

  ReturnError:

  SetSgError("Internal Error: StartFixAxes()");
  return -1;
}


static int FindInvertableMx(const int *Mx, int *InvMx,
                            int *nActive, int *irActive, int *icActive)
{
  int  Init, deterMx, i;


  if (*nActive == 0 || *nActive == 3)
    return 0;

  if (*nActive == -1)
  {
    Init = 1;

        deterMx = deterRotMx(Mx);
    if (deterMx)
    {
      InverseRotMx(Mx, InvMx);

      *nActive = 3;
      return deterMx;
    }
  }
  else
    Init = 0;

  if (Init || *nActive == 2)
  {
    for (;;)
    {
      if (Init)
      {
        irActive[0] = 0;
        irActive[1] = 1;
        icActive[0] = 0;
        icActive[1] = 1;
        Init = 0;
      }
      else
      {
        if (++icActive[1] == 3) {
          if (++icActive[0] == 2) {
            if (++irActive[1] == 3) {
              if (++irActive[0] == 2) {
                Init = 1;
                break;
              }
              else {
                irActive[1] = irActive[0] + 1;
                icActive[0] = 0;
                icActive[1] = 1;
              }
            }
            else {
              icActive[0] = 0;
              icActive[1] = 1;
            }
          }
          else {
            icActive[1] = icActive[0] + 1;
          }
        }
      }

      InvMx[0] =   Mx[irActive[1] * 3 + icActive[1]];
      InvMx[1] = - Mx[irActive[0] * 3 + icActive[1]];
      InvMx[2] = - Mx[irActive[1] * 3 + icActive[0]];
      InvMx[3] =   Mx[irActive[0] * 3 + icActive[0]];

          deterMx = InvMx[3] * InvMx[0] - InvMx[1] * InvMx[2];
      if (deterMx) {
        *nActive = 2;
        return deterMx;
      }
    }
  }

  if (*nActive == 2)
    return 0;

  if (Init) i = 0;
  else      i = irActive[0] * 3 + icActive[0] + 1;

  for ( ; i < 9; i++)
  {
    if (Mx[i]) {
      irActive[0] = i / 3;
      icActive[0] = i % 3;
      *nActive = 1;
      return Mx[i];
    }
  }

  if (*nActive == 1)
    return 0;

  *nActive = 0;
  return 1;
}


static int SetInvCBMxT(const int *CBMxT, const int *InvCBMxR, int *InvCBMxT)
{
  int  i;


  RotMx_t_Vector(InvCBMxT, InvCBMxR, CBMxT, CRBF * CTBF);

  for (i = 0; i < 3; i++)
  {
    if (InvCBMxT[i] % CRBF) {
      SetSgError("Internal Error: SetInvCBMxT()");
      return -1;
    }

    if (InvCBMxT[i])
        InvCBMxT[i] = CTBF - InvCBMxT[i] / CRBF;
  }

  return 0;
}


static int FixAxes(const T_SgInfo *SgInfo,
                   const T_SgInfo *GenSgI, const int *iGen,
                   T_RTMx *CBMx, T_RTMx *InvCBMx,
                   int *FreeMx, int TryAgain)
{
  int        i, NextTryAgain;
  int        IniCBMxT[3], SingleFreeMx[9];
  T_RTMx     SMxG, SMxS_G;
  int        NextFreeMxBuf[9], R_I_FMxBuf[9];
  int        R_I[9], *R_I_FMx, InvR_I_FMx[9], deterR_I_FMx;
  int        S_G[3], CmpS_G[3], RedSh[3], Sh[3], *NextFreeMx;
  int        nActive, irActive[3], icActive[3];
  int        nTrV, iTrV;
  const int  *TrV;

  icActive[0]=icActive[1]=icActive[2]=0;
  irActive[0]=irActive[1]=irActive[2]=0;

  if (FreeMx == NULL) {
    for (i = 0; i < 3; i++) {
         CBMx->s.T[i] = 0;
      InvCBMx->s.T[i] = 0;
    }
  }

  i = StartFixAxes(SgInfo, GenSgI, iGen, CBMx, InvCBMx,
                   &SMxG, &SMxS_G, SingleFreeMx, &NextTryAgain);
  if (i != 1)
    return i;

  if (FreeMx) {
    RotMxMultiply(NextFreeMxBuf, SingleFreeMx, FreeMx);
    NextFreeMx =  NextFreeMxBuf;
  }
  else
    NextFreeMx = SingleFreeMx;

  for (i = 0; i < 9; i++)
    R_I[i] = SMxG.s.R[i];

  for (i = 0; i < 9; i += 4)
    R_I[i] -= 1;

  if (FreeMx) {
    RotMxMultiply(R_I_FMxBuf, R_I, FreeMx);
    R_I_FMx =     R_I_FMxBuf;
  }
  else
    R_I_FMx = R_I;

  for (i = 0; i < 3; i++)
    IniCBMxT[i] = CBMx->s.T[i];

  nActive = -1;

  for (;;)
  {
    deterR_I_FMx = FindInvertableMx(R_I_FMx, InvR_I_FMx,
                                    &nActive, irActive, icActive);
    if (deterR_I_FMx == 0)
      break;

    nTrV = GenSgI->LatticeInfo->nTrVector;
     TrV = GenSgI->LatticeInfo->TrVector;

    for (iTrV = 0; iTrV < nTrV; iTrV++, TrV += 3)
    {
      for (i = 0; i < 3; i++) {
        S_G[i] =   (CTBF / STBF)
                 * ((SMxS_G.s.T[i] - SMxG.s.T[i] - TrV[i]) % STBF);
        RedSh[i] = 0;
      }

      switch(nActive)
      {
        case 1:
          RedSh[icActive[0]] = S_G[irActive[0]];
          break;
        case 2:
          RedSh[icActive[0]] =   InvR_I_FMx[0] * S_G[irActive[0]]
                               + InvR_I_FMx[1] * S_G[irActive[1]];
          RedSh[icActive[1]] =   InvR_I_FMx[2] * S_G[irActive[0]]
                               + InvR_I_FMx[3] * S_G[irActive[1]];
          break;
        case 3:
          RotMx_t_Vector(RedSh, InvR_I_FMx, S_G, 0);
          break;
        default:
          break;
      }

      if (FreeMx)
      {
        RotMx_t_Vector(Sh, FreeMx, RedSh, 0);

        for (i = 0; i < 3; i++)
          Sh[i] %= (CTBF * abs(deterR_I_FMx));
      }
      else
      {
        for (i = 0; i < 3; i++)
          Sh[i] = RedSh[i] % (CTBF * abs(deterR_I_FMx));
      }

      RotMx_t_Vector(CmpS_G, R_I, Sh, 0);

      for (i = 0; i < 3; i++)
        if ((CmpS_G[i] - S_G[i] * deterR_I_FMx) % (CTBF * abs(deterR_I_FMx)))
          break;

      if (i < 3)
        continue;

      if (deterR_I_FMx != 1)
      {
        for (i = 0; i < 3; i++)
        {
          if (Sh[i] % abs(deterR_I_FMx))
            goto ReturnError;

          Sh[i] /= deterR_I_FMx;
        }
      }

      for (i = 0; i < 3; i++) {
            CBMx->s.T[i] = IniCBMxT[i] + Sh[i] % CTBF;
        if (CBMx->s.T[i] < 0)
            CBMx->s.T[i] += CTBF;
      }

      if (SetInvCBMxT(CBMx->s.T, InvCBMx->s.R, InvCBMx->s.T) != 0)
        return -1;

      if (iGen[1] == 0)
        return 1;

          i = FixAxes(SgInfo, GenSgI, &iGen[1], CBMx, InvCBMx,
                      NextFreeMx, NextTryAgain);
      if (i != 0)
        return i;
    }

    if (TryAgain == 0)
      break;
  }

  return 0;

  ReturnError:

  SetSgError("Internal Error: FixAxes()");
  return -1;
}


static int CompleteCBMx(const T_SgInfo *SgInfo, const T_LatticeInfo *NewLI,
                        const T_SgInfo *GenSgI,
                        const int *IniCBMxR, const int *IniInvCBMxR,
                        T_RTMx       *CBMx,  T_RTMx       *InvCBMx)
{
  int  iGen[5], i;


  if (SgInfo->XtalSystem == XS_Triclinic)
  {
    for (i = 0; i < 9; i++) {
         CBMx->s.R[i] =    IniCBMxR[i];
      InvCBMx->s.R[i] = IniInvCBMxR[i];
    }

    if (GenSgI->PointGroup == PG_1)
    {
      for (i = 0; i < 3; i++) {
           CBMx->s.T[i] = 0;
        InvCBMx->s.T[i] = 0;
      }
      return 1;
    }

    iGen[0] = -1;
    iGen[1] =  0;

    return FixAxes(SgInfo, GenSgI, iGen, CBMx, InvCBMx, NULL, 0);
  }

  if (SgInfo->XtalSystem == XS_Monoclinic)
  {
    int        iCCs, BufRMx[9];
    int        RMxCCs_Buf[9], RMxCCn_Buf[9], InvRMxCCn_Buf[9], RotLTrV[3];
    const int  *RMxAA, *RMxCCs, *RMxCCn, *InvRMxCCn, *TrV;
    T_RTMx     BufCBMx, BufInvCBMx;


    if (NewLI->nTrVector != 1 && NewLI->nTrVector != 2)
      goto ReturnError;

    for (i = 0; i < 9; i++) {
         BufCBMx.s.R[i] =    IniCBMxR[i];
      BufInvCBMx.s.R[i] = IniInvCBMxR[i];
    }

        i = AlignUniqueAxis(SgInfo, GenSgI,
                            BufCBMx.s.R, BufInvCBMx.s.R, &RMxAA);
    if (i != 1)
      return i;

    if (GenSgI->nList < 2)
      goto ReturnError;

    for (i = 0; i < 9; i++) {
      RMxCCs_Buf[i] = RMx_2_110[i];
      RMxCCn_Buf[i] = RMx_3_001[i];
    }

    switch (GenSgI->ListRotMxInfo[1].RefAxis)
    {
      case 'z': break;
      case 'x': RotateRotMx(RMxCCs_Buf, RMx_3_111, RMx_3i111);
                RotateRotMx(RMxCCn_Buf, RMx_3_111, RMx_3i111);
                break;
      case 'y': RotateRotMx(RMxCCs_Buf, RMx_3i111, RMx_3_111);
                RotateRotMx(RMxCCn_Buf, RMx_3i111, RMx_3_111);
                break;
      default:
        goto ReturnError;
    }

    InverseRotMx(RMxCCn_Buf, InvRMxCCn_Buf);

                                           i = 0;
                                      iGen[i++] =  1;
    if (GenSgI->PointGroup == PG_2_m) iGen[i++] = -1;
                                      iGen[i  ] =  0;

    RMxCCs = RMx_1_000;

    for (iCCs = 0; iCCs < 2; iCCs++, RMxCCs = RMxCCs_Buf)
    {
      RMxCCn = InvRMxCCn = RMx_1_000;

      for (;;)
      {
        if (NewLI->nTrVector == 2)
        {
          RotMx_t_Vector(RotLTrV, RMxAA,  &NewLI->TrVector[3], STBF);
          RotMx_t_Vector(BufRMx,  RMxCCn, RotLTrV,             STBF);
          RotMx_t_Vector(RotLTrV, RMxCCs, BufRMx,              STBF);

          TrV = &GenSgI->LatticeInfo->TrVector[3];

          for (i = 0; i < 3; i++)
            if (RotLTrV[i] != TrV[i])
              break;
        }

        if (NewLI->nTrVector == 1 || i == 3)
        {
          RotMxMultiply(BufRMx,    RMxCCn, BufCBMx.s.R);
          RotMxMultiply(CBMx->s.R, RMxCCs, BufRMx);

          RotMxMultiply(BufRMx,    BufInvCBMx.s.R, InvRMxCCn);
          RotMxMultiply(InvCBMx->s.R, BufRMx,         RMxCCs);

              i = FixAxes(SgInfo, GenSgI, iGen, CBMx, InvCBMx, NULL, 0);
          if (i != 0)
            return i;
        }

        if      (RMxCCn ==    RMx_1_000) {
                 RMxCCn =     RMxCCn_Buf; InvRMxCCn = InvRMxCCn_Buf; }
        else if (RMxCCn ==    RMxCCn_Buf) {
                 RMxCCn =  InvRMxCCn_Buf; InvRMxCCn =    RMxCCn_Buf; }
        else {
                 RMxCCn = NULL;
                 break;
        }
      }
    }

    return 0;
  }

  for (i = 0; i < 9; i++) {
       CBMx->s.R[i] =    IniCBMxR[i];
    InvCBMx->s.R[i] = IniInvCBMxR[i];
  }

  if (SgInfo->XtalSystem == XS_Orthorhombic)
  {
    int        iNextBasis;
    int        BufCBMxR[9], BufInvCBMxR[9];
    int         NLTrV_Buf1[3], NLTrV_Buf2[3];
    const int  *NLTrV, *GLTrV;


    if ((GenSgI->LatticeInfo->Code == 'I') != (NewLI->Code == 'I'))
      return 0;

    if (   NewLI->Code == 'A'
        || NewLI->Code == 'B'
        || NewLI->Code == 'C') {
      NLTrV =               &NewLI->TrVector[3];
      GLTrV = &GenSgI->LatticeInfo->TrVector[3]; }
    else {
      NLTrV = NULL;
      GLTrV = NULL; }
                                           i = 0;
                                      iGen[i++] =  1;
                                      iGen[i++] =  2;
    if (GenSgI->PointGroup == PG_mmm) iGen[i++] = -1;
                                      iGen[i  ] =  0;

    for (iNextBasis = 0; iNextBasis < 6; iNextBasis++)
    {
      if      (iNextBasis % 2)
      {
        RotMxMultiply(   BufCBMxR, RMx_2_110,    CBMx->s.R);
        RotMxMultiply(BufInvCBMxR, InvCBMx->s.R, RMx_2_110);

        for (i = 0; i < 9; i++) {
             CBMx->s.R[i] =    BufCBMxR[i];
          InvCBMx->s.R[i] = BufInvCBMxR[i];
        }
      }
      else if (iNextBasis == 2) {
        RotMxMultiply(   CBMx->s.R, RMx_3_111,   IniCBMxR);
        RotMxMultiply(InvCBMx->s.R, IniInvCBMxR, RMx_3i111);
      }
      else if (iNextBasis) {
        RotMxMultiply(   CBMx->s.R, RMx_3i111,   IniCBMxR);
        RotMxMultiply(InvCBMx->s.R, IniInvCBMxR, RMx_3_111);
      }

      if (NLTrV)
      {
        if      (iNextBasis % 2) {
          RotMx_t_Vector(NLTrV_Buf1, RMx_2_110, NLTrV, STBF);
          NLTrV = NLTrV_Buf1;
        }
        else if (iNextBasis == 2) {
          RotMx_t_Vector(NLTrV_Buf2, RMx_3_111, &NewLI->TrVector[3], STBF);
          NLTrV = NLTrV_Buf2;
        }
        else if (iNextBasis) {
          RotMx_t_Vector(NLTrV_Buf2, RMx_3i111, &NewLI->TrVector[3], STBF);
          NLTrV = NLTrV_Buf2;
        }

        for (i = 0; i < 3; i++)
          if (NLTrV[i] != GLTrV[i])
            break;

        if (i < 3)
          continue;
      }

          i = FixAxes(SgInfo, GenSgI, iGen, CBMx, InvCBMx, NULL, 0);
      if (i != 0)
        return i;
    }
  }

  if (SgInfo->XtalSystem == XS_Tetragonal)
  {
        i = AlignUniqueAxis(SgInfo, GenSgI, CBMx->s.R, InvCBMx->s.R, NULL);
    if (i != 1)
      return -1;
                          i = 0;
                     iGen[i++] =  1;

    switch (GenSgI->PointGroup) {
      case PG_422:
      case PG_4mm:
      case PG_4b2m:
      case PG_4bm2:
      case PG_4_mmm: iGen[i++] =  2;
    }

    switch (GenSgI->PointGroup) {
      case PG_4_m:
      case PG_4_mmm: iGen[i++] = -1;
    }
                     iGen[i  ] =  0;

    return FixAxes(SgInfo, GenSgI, iGen, CBMx, InvCBMx, NULL, 0);
  }

  if (SgInfo->XtalSystem == XS_Trigonal)
  {
        i = AlignUniqueAxis(SgInfo, GenSgI, CBMx->s.R, InvCBMx->s.R, NULL);
    if (i != 1)
      return i;
                         i = 0;

    switch (GenSgI->PointGroup) {
      case PG_3:
      case PG_312:
      case PG_32:
      case PG_3m1:
      case PG_3m:   iGen[i++] =  1;
                    break;
      case PG_3b:
      case PG_3bm1:
      case PG_3b1m:
      case PG_3bm:  iGen[i++] = -3;
    }

    switch (GenSgI->PointGroup) {
      case PG_321:
      case PG_312:
      case PG_32:
      case PG_3m1:
      case PG_31m:
      case PG_3m:
      case PG_3bm1:
      case PG_3b1m:
      case PG_3bm:  iGen[i++] =  2;
    }

    switch (GenSgI->PointGroup) {
      case PG_321:
      case PG_31m:  iGen[i++] =  1;
    }
                    iGen[i  ] =  0;

    return FixAxes(SgInfo, GenSgI, iGen, CBMx, InvCBMx, NULL, 0);
  }

  if (SgInfo->XtalSystem == XS_Hexagonal)
  {
        i = AlignUniqueAxis(SgInfo, GenSgI, CBMx->s.R, InvCBMx->s.R, NULL);
    if (i != 1)
      return -1;
                         i = 0;

    switch (GenSgI->PointGroup) {
      case PG_6bm2:
      case PG_6b2m: iGen[i++] =  2;
    }
                    iGen[i++] =  1;

    switch (GenSgI->PointGroup) {
      case PG_622:
      case PG_6mm:
      case PG_6_mmm: iGen[i++] =  2;
    }

    switch (GenSgI->PointGroup) {
      case PG_6_m:
      case PG_6_mmm: iGen[i++] = -1;
    }
                     iGen[i  ] =  0;

    return FixAxes(SgInfo, GenSgI, iGen, CBMx, InvCBMx, NULL, 0);
  }

  if (SgInfo->XtalSystem == XS_Cubic)
  {                                            i = 0;
                                          iGen[i++] =  3;
                                          iGen[i++] =  1;
                                          iGen[i++] =  2;
    if (   GenSgI->PointGroup == PG_m3b
        || GenSgI->PointGroup == PG_m3bm) iGen[i++] = -1;
                                          iGen[i  ] =  0;

    return FixAxes(SgInfo, GenSgI, iGen, CBMx, InvCBMx, NULL, 0);
  }

  return 0;

  ReturnError:

  SetSgError("Internal Error: CompleteCBMx()");
  return -1;
}


const T_TabSgName *FindReferenceSpaceGroup(T_SgInfo *SgInfo,
                                           T_RTMx *CBMx, T_RTMx *InvCBMx)
{
  int                  stat, NewPG, SgInfo_CI, OL_SgInfo, OL_GenSgI;
  const T_TabSgName    *tsgn;
  T_SgInfo             GenSgI;
  T_RTMx               GenSgI_ListSeitzMx[5];
  T_RotMxInfo          GenSgI_ListRotMxInfo[5];
  int                  iList, PrevSgNumber;
  int                  FacIniCBMxR;
  T_RotMxInfo          *lrmxi;
  const T_LatticeInfo  *NewLI;
  int                  IniCBMxR[9], IniInvCBMxR[9];


  GenSgI.MaxList       = 5;
  GenSgI.ListSeitzMx   = GenSgI_ListSeitzMx;
  GenSgI.ListRotMxInfo = GenSgI_ListRotMxInfo;

      FacIniCBMxR = InitialCBMxR(SgInfo, &NewLI, &NewPG, IniCBMxR, IniInvCBMxR);
  if (FacIniCBMxR < 0)
    return NULL;

      OL_SgInfo = SgInfo->OrderL;
  if (OL_SgInfo % FacIniCBMxR)
    goto ReturnError;

  OL_SgInfo /= FacIniCBMxR;

  SgInfo_CI = (SgInfo->Centric || SgInfo->InversionOffOrigin);

  PrevSgNumber = 0;

  for (tsgn = TabSgName; tsgn->HallSymbol; tsgn++)
  {
    if (tsgn->HallSymbol[1] == 'R')
      continue;

    if (VolAPointGroups[tsgn->SgNumber] != NewPG)
      continue;

    if (tsgn->SgNumber == PrevSgNumber)
      continue;

    PrevSgNumber = tsgn->SgNumber;

    InitSgInfo(&GenSgI);
    GenSgI.GenOption = -1;

    ParseHallSymbol(tsgn->HallSymbol, &GenSgI);

    if (SgError != NULL)
      return NULL;

    if (ApplyOriginShift(&GenSgI) < 0)
      return NULL;

    if (SgInfo_CI != (GenSgI.Centric || GenSgI.InversionOffOrigin))
      goto ReturnError;

    OL_GenSgI = GenSgI.LatticeInfo->nTrVector;

    if (SgInfo_CI)
      OL_GenSgI *= 2;

    lrmxi = &GenSgI.ListRotMxInfo[1];

    for (iList = 1; iList < GenSgI.nList; iList++, lrmxi++)
    {
      OL_GenSgI *= abs(lrmxi->Order);

      if (   (lrmxi->Order == -1 || lrmxi->Order == -3)
          && GenSgI.Centric == 0 && GenSgI.InversionOffOrigin == 0)
        goto ReturnError;
    }

    if (OL_GenSgI == OL_SgInfo)
    {
      if (NewLI->nTrVector != GenSgI.LatticeInfo->nTrVector)
        goto ReturnError;

      GenSgI.PointGroup = NewPG;

#if DEBUG_FindConventionalSetting
      fprintf(stdout, "%s ?= %s (%d)\n",
        SgInfo->HallSymbol, tsgn->HallSymbol, tsgn->SgNumber);
#endif

      stat = CompleteCBMx(SgInfo, NewLI, &GenSgI,
                          IniCBMxR, IniInvCBMxR,
                             CBMx,     InvCBMx);
      if (stat < 0)
        return NULL;

      if (stat)
        return tsgn;
    }
  }

  SetSgError("Internal Error: Space Group not found");
  return NULL;

  ReturnError:

  SetSgError("Internal Error: FindReferenceSpaceGroup()");
  return NULL;
}
