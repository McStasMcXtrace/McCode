/*
  Space Group Info's (c) 1994-96 Ralf W. Grosse-Kunstleve
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>


#define SGCOREDEF__
#include "sginfo.h"


typedef struct
  {
    int         OriginChoice;
    int         CellChoice;
    int         BasisChoice;
    const char  *BT_or_UA;
  }
  T_ExtInfo;


static const char *Ext_BT_or_UA[] =
  {
    /*  0 */ "abc",
    /*  1 */ "ba-c",
    /*  2 */ "cab",
    /*  3 */ "-cba",
    /*  4 */ "bca",
    /*  5 */ "a-cb",
    /*  6 */         "bac", /* 6 -> 1 */
    /*  7 */         "cba", /* 7 -> 3 */
    /*  8 */         "acb", /* 8 -> 5 */
    /*  9 */ "-b", "b-", "bb", "bb", /* 10, 11, 12 ->  9 */
    /* 13 */ "-c", "c-", "bc", "cb", /* 14, 15, 16 -> 13 */
    /* 17 */ "-a", "a-", "ba", "ab", /* 18, 19, 20 -> 17 */
    /* 21 */ "b",
    /* 22 */ "c",
    /* 23 */ "a",
    NULL
  };


typedef struct
    {
      int     Improper, Rotation, RefAxis, DirCode, Screw;
      T_RTMx  SeitzMx;
    }
    T_HallGenerator;


#define SkipWhite(cp) while (*(cp) && (*(cp) == '_' || isspace(*(cp)))) (cp)++


static const char *IErr_Corrupt_TabSgName =
   "Internal Error: Corrupt TabSgName";


static int FindSchoenfliesSymbol(const char *SfSymbol)
{
  int         SgNumber;
  const char  **TabSymbol;
  const char  *s, *t;


  TabSymbol = SchoenfliesSymbols + 1;

  for (SgNumber = 1; SgNumber <= 230; SgNumber++)
  {
    t = *TabSymbol;
    s = SfSymbol;

    while (*t && *s)
    {
      if (   toupper(*t) != toupper(*s)
          && (*t != '^' || isalpha(*s) || isdigit(*s)))
        break;

      t++;
      s++;
    }

    if (*t == *s)
      return SgNumber;

    TabSymbol++;
  }

  return -1;
}


static int SgLabelCmp(const int SgNumber,
                      const char *SgLabel, const char *WtdLbl)
{
  const char  *sgl, *wl;


  /* first try: plain strcmp
   */
  sgl = SgLabel;

  for (wl = WtdLbl; ; wl++)
  {
    SkipWhite(wl);
    SkipWhite(sgl);

    if (*sgl == '\0' || *sgl == '=')
    {
      if (*wl == '\0') return 0;
      break;
    }

    if (*sgl == '-')
    {
      if (*wl != '-' && toupper(*wl) != 'B')
        break;
    }
    else if (toupper(*sgl) != toupper(*wl))
      break;

    sgl++;
  }

  /* second try: swap the dash (there should be only one)
   */
  sgl = SgLabel;

  for (wl = WtdLbl; ; wl++)
  {
    SkipWhite(wl);
    SkipWhite(sgl);

    if (*sgl == '-')
    {
      if (wl[1] != '-' && toupper(wl[1]) != 'B')
        break;
      if (toupper(sgl[1]) != toupper(*wl))
        break;

      sgl++;
      wl++;
    }
    else
    {
      if (*sgl == '\0' || *sgl == '=')
      {
        if (*wl == '\0') return 0;
        break;
      }

      if (toupper(*sgl) != toupper(*wl))
        break;
    }

    sgl++;
  }

  if (SgNumber >= 195) /* cubic space groups only */
  {
    /* third try: ignore the "-3" dash
     */
    sgl = SgLabel;

    for (wl = WtdLbl; ; wl++)
    {
      SkipWhite(wl);
      SkipWhite(sgl);

      if (*sgl == '-' && sgl[1] == '3')
        sgl++;

      if (*sgl == '\0' || *sgl == '=')
      {
        if (*wl == '\0') return 0;
        break;
      }

      if (toupper(*sgl) != toupper(*wl))
        break;

      sgl++;
    }
  }

  return -1;
}


static int ParseExtension(const char *Ext, T_ExtInfo *ExtInfo)
{
  int         i, mode;
  const char  *e, *t;


  ExtInfo->OriginChoice =
  ExtInfo->CellChoice =
  ExtInfo->BasisChoice = ' ';
  ExtInfo->BT_or_UA = "";

  mode = 0;

  while (*Ext)
  {
    if      (strchr("12",   *Ext) != NULL)
    {
      ExtInfo->CellChoice   =
      ExtInfo->OriginChoice = *Ext++;
    }
    else if (strchr("3",    *Ext) != NULL)
    {
      ExtInfo->CellChoice   = *Ext++;
    }
    else if (strchr("Ss",   *Ext) != NULL)
    {
      ExtInfo->OriginChoice = '1';
      Ext++;
    }
    else if (strchr("Zz",   *Ext) != NULL)
    {
      ExtInfo->OriginChoice = '2';
      Ext++;
    }
    else if (strchr("Hh",   *Ext) != NULL)
    {
      ExtInfo->BasisChoice = 'H';
      Ext++;
    }
    else if (strchr("Rr",   *Ext) != NULL)
    {
      ExtInfo->BasisChoice = 'R';
      Ext++;
    }
    else if (mode == 0)
      mode = 1;

    if (mode == 2)
      break;

    for (i = 0; Ext_BT_or_UA[i]; i++)
    {
      for (e = Ext, t = Ext_BT_or_UA[i]; *t; e++, t++)
        if (toupper(*e) != toupper(*t))
          break;

      if (*t == '\0')
      {
        if      (6 <= i && i <=  8)
          i = 2 * i - 11;
        else if (9 <= i && i <= 20)
          i = 9 + ((i - 9) / 4) * 4;

        ExtInfo->BT_or_UA = Ext_BT_or_UA[i];
        Ext = e;
        break;
      }
    }

    if (mode == 0)
      break;

    mode = 2;
  }

  if (*Ext)
    return -1;

  return 0;
}


static void ExpandMonoclinic(int unique_axis, const char *o, char *m)
{
  if (*o) *m++ = *o++;

  switch (tolower(unique_axis))
  {
    case 'a':
      while (*o) *m++ = *o++;
      *m++ = '1';
      *m++ = '1';
      break;
    case 'c':
      *m++ = '1';
      *m++ = '1';
      while (*o) *m++ = *o++;
      break;
    default:
      *m++ = '1';
      while (*o) *m++ = *o++;
      *m++ = '1';
      break;
  }

  *m = '\0';
}


const T_TabSgName *FindTabSgNameEntry(const char *UserSgName, int VolLetter)
{
#define                   MaxWtdLbl 20
  char     WtdLblOriginal[MaxWtdLbl    + 1];
  char     WtdLblModified[MaxWtdLbl    + 1];
  char    *WtdLbl;
#define          MaxWtdExt  5
  char    WtdExt[MaxWtdExt    + 1];
  int     WtdSgNumber;
  int     WtdLblOriginChoice;
  int     WtdLblBasisChoice;
  int     iwl, iwe;
  char    *wl, *we;

  int                i, IsExpanded, lbl_match;
  const char         *sgl;
  const T_TabSgName  *tsgn;
  int                 WtdCC;
  const char         *WtdUA;
  char                WtdUA_Buf[2];
  T_ExtInfo          ExtInfo, WtdExtInfo;


  if      (VolLetter == 0 || isspace(VolLetter))
    VolLetter = 'A';
  else if (VolLetter == '1')
    VolLetter = 'I';
  else
  {
           VolLetter = toupper(VolLetter);
    if (   VolLetter != 'I'
        && VolLetter != 'A')
      return NULL;
  }

  WtdLbl = WtdLblOriginal;

   wl = WtdLbl;
  iwl = 0;

  while (*UserSgName && *UserSgName != ':')
  {
    if (isspace(*UserSgName) == 0 && *UserSgName != '_')
    {
      if (iwl >= MaxWtdLbl)
        return NULL;

      *wl++ = *UserSgName;
      iwl++;
    }

    UserSgName++;
  }

  *wl = '\0';

  if (iwl == 0)
    return NULL;

   we = WtdExt;
  iwe = 0;
  *we = '\0';

  if (*UserSgName)
  {
    UserSgName++;

    while (*UserSgName)
    {
      if (isspace(*UserSgName) == 0 && *UserSgName != '_')
      {
        if (iwe >= MaxWtdExt)
          return NULL;

        *we++ = *UserSgName;
        iwe++;
      }

      UserSgName++;
    }
  }

  *we = '\0';

  WtdLblOriginChoice = ' ';
  WtdLblBasisChoice  = ' ';

  if (iwl > 1)
  {
    wl = &WtdLbl[iwl - 1];

    if      (*wl == 'S' || *wl == 's')
    { WtdLblOriginChoice = '1'; *wl = '\0'; iwl--; }
    else if (*wl == 'Z' || *wl == 'z')
    { WtdLblOriginChoice = '2'; *wl = '\0'; iwl--; }
    else if (*wl == 'H' || *wl == 'h')
    { WtdLblBasisChoice  = 'H'; *wl = '\0'; iwl--; }
    else if (*wl == 'R' || *wl == 'r')
    { WtdLblBasisChoice  = 'R'; *wl = '\0'; iwl--; }
  }

  if (isalpha(WtdLbl[0]))
    WtdSgNumber = FindSchoenfliesSymbol(WtdLbl);
  else
  {
    for (wl = WtdLbl; *wl; wl++)
      if (isdigit(*wl) == 0)
        return NULL;

    if (   sscanf(WtdLbl, "%d", &WtdSgNumber) != 1
        || WtdSgNumber <   1
        || WtdSgNumber > 230)
      return NULL;
  }

  if (ParseExtension(WtdExt, &WtdExtInfo) != 0)
    return NULL;

  if      (WtdExtInfo.OriginChoice == ' ')
           WtdExtInfo.OriginChoice =  WtdLblOriginChoice;
  else if (WtdExtInfo.OriginChoice != WtdLblOriginChoice
                                   && WtdLblOriginChoice != ' ')
    return NULL;

  if      (WtdExtInfo.BasisChoice == ' ')
           WtdExtInfo.BasisChoice =  WtdLblBasisChoice;
  else if (WtdExtInfo.BasisChoice != WtdLblBasisChoice
                                  && WtdLblBasisChoice != ' ')
    return NULL;

  if (   WtdExtInfo.OriginChoice != ' '
      && WtdExtInfo.BasisChoice  != ' ')
    return NULL;

  for (IsExpanded = 0; IsExpanded < 4; IsExpanded++)
  {
    for (tsgn = TabSgName; tsgn->HallSymbol; tsgn++)
    {
      if (   IsExpanded != 0
          && tsgn->SgNumber > 15)
        break;

      lbl_match = 0;

      if (WtdSgNumber == -1)
      {
        i = 1;
                sgl = tsgn->SgLabels;
        while (*sgl && i <= 2)
        {
          while (*sgl && strchr(" =\t", *sgl) != NULL) sgl++;

          if (SgLabelCmp(tsgn->SgNumber, sgl, WtdLbl) == 0)
          {
            lbl_match = i;
            break;
          }

          while (*sgl && strchr(" =\t", *sgl) == NULL) sgl++;

          i++;
        }
      }

      if (ParseExtension(tsgn->Extension, &ExtInfo) != 0) {
        SetSgError(IErr_Corrupt_TabSgName);
        return NULL;
      }

      if (WtdSgNumber == tsgn->SgNumber || lbl_match != 0)
      {
        if (   tsgn->SgNumber >=  3
            && tsgn->SgNumber <  16)
        {
          if (   WtdLblOriginChoice != ' '
              || WtdExtInfo.BasisChoice != ' '
              || (int) strlen(WtdExtInfo.BT_or_UA) > 2)
            continue; /* next tsgn */

          if (WtdSgNumber == tsgn->SgNumber)
          {
            if (WtdExtInfo.BT_or_UA[0])
              WtdUA = WtdExtInfo.BT_or_UA;
            else if (VolLetter == 'I')
            {
              if (   ExtInfo.BT_or_UA[0] != 'c'
                  && ExtInfo.BT_or_UA[1] != 'c')
                continue; /* next tsgn */

              if (   ExtInfo.CellChoice == ' '
                  && (   WtdExtInfo.CellChoice == ' '
                      || WtdExtInfo.CellChoice == '1'))
                return tsgn;

              i = 0;
              for (sgl = tsgn->SgLabels; *sgl; sgl++)
                if (*sgl == '=') i++;

              if (   i == 2
                  && (   WtdExtInfo.CellChoice == ' '
                      || WtdExtInfo.CellChoice == ExtInfo.CellChoice))
                return tsgn;

              continue; /* next tsgn */
            }
            else
              WtdUA = "b";
          }
          else /* if (lbl_match != 0) */
          {
            if (WtdExtInfo.BT_or_UA[0])
              WtdUA = WtdExtInfo.BT_or_UA;
            else if (lbl_match > 1)
              WtdUA = ExtInfo.BT_or_UA;
            else if (   VolLetter == 'I'
                     && ExtInfo.CellChoice == ' ')
              WtdUA = "c";
            else
              WtdUA = "b";
          }

          if (WtdExtInfo.CellChoice != ' ')
            WtdCC = WtdExtInfo.CellChoice;
          else if (ExtInfo.CellChoice == '1')
            WtdCC = ExtInfo.CellChoice;
          else
            WtdCC = ' ';

          if (strcmp(ExtInfo.BT_or_UA, WtdUA) == 0)
          {
            if (WtdCC == ' ' && lbl_match > 1)
              return tsgn;
            if (ExtInfo.CellChoice == WtdCC)
              return tsgn;
            if (ExtInfo.CellChoice == ' ' && WtdCC == '1')
              return tsgn;
            if (ExtInfo.CellChoice == '1' && WtdCC == ' ')
              return tsgn;
          }
        }
        else if (ExtInfo.BasisChoice != ' ')
        {
          if (   WtdExtInfo.OriginChoice != ' '
              || WtdExtInfo.CellChoice != ' '
              || WtdExtInfo.BT_or_UA[0] != '\0')
            continue; /* next tsgn */

          if (ExtInfo.BasisChoice == WtdExtInfo.BasisChoice)
            return tsgn;

          if (WtdExtInfo.BasisChoice == ' ')
          {
            if (ExtInfo.BasisChoice == 'R' && VolLetter == 'I')
              return tsgn;
            if (ExtInfo.BasisChoice == 'H' && VolLetter != 'I')
              return tsgn;
          }
        }
        else if (WtdExtInfo.BasisChoice == ' ')
        {
          if ( (WtdExtInfo.OriginChoice == ' ' && ExtInfo.OriginChoice == '1')
            || (WtdExtInfo.OriginChoice == '1' && ExtInfo.OriginChoice == ' ')
            ||  WtdExtInfo.OriginChoice ==        ExtInfo.OriginChoice)
          {
            if (WtdExtInfo.BT_or_UA[0])
            {
              if (WtdExtInfo.BT_or_UA == ExtInfo.BT_or_UA)
                return tsgn;
              if (   WtdExtInfo.BT_or_UA == Ext_BT_or_UA[0]
                  &&    ExtInfo.BT_or_UA[0] == '\0')
                return tsgn;
            }
            else
            {
              if (lbl_match != 0)
                return tsgn;
              if (ExtInfo.BT_or_UA[0] == '\0')
                return tsgn;
            }
          }
        }
      }
    }

    if (WtdSgNumber != -1)
      return NULL;

    if ((int) strlen(WtdExtInfo.BT_or_UA) > 2)
      return NULL;

    if (IsExpanded == 0)
    {
      iwl += 2;

      if (iwl > MaxWtdLbl)
        IsExpanded = 2;
      else
      {
        if (WtdExtInfo.BT_or_UA[0])
          WtdUA = WtdExtInfo.BT_or_UA;
        else
        {
          if (VolLetter == 'I')
            WtdUA = "c";
          else
            WtdUA = "b";
        }

        ExpandMonoclinic(WtdUA[0], WtdLblOriginal, WtdLblModified);

        WtdLbl = WtdLblModified;
      }
    }
    else if (IsExpanded == 1)
    {
      if (WtdExtInfo.BT_or_UA[0])
        return NULL;

      if (VolLetter == 'I')
        WtdUA = "b";
      else
        WtdUA = "c";

      ExpandMonoclinic(WtdUA[0], WtdLblOriginal, WtdLblModified);
    }

    if (IsExpanded == 2)
    {
      if (WtdExtInfo.BT_or_UA[0])
        return NULL;

      iwl -= 2;

      if (iwl < 2)
        return NULL;
                                            iwl--;
      WtdUA_Buf[0] = tolower(WtdLblOriginal[iwl]);
                             WtdLblOriginal[iwl] = '\0';
      WtdUA_Buf[1] = '\0';

      if (strchr("abc", WtdUA_Buf[0]) == NULL)
        return NULL;

      WtdUA = WtdUA_Buf;

      iwl += 2;

      if (iwl > MaxWtdLbl)
        return NULL;

      ExpandMonoclinic(WtdUA[0], WtdLblOriginal, WtdLblModified);

      WtdLbl = WtdLblModified;
    }
  }

  return NULL;
}


unsigned int SgID_Number(const T_TabSgName *tsgn)
{
  unsigned int  ID;
  int           iBT;
  const char    *UA;
  T_ExtInfo     ExtInfo;


  ID = tsgn->SgNumber;

  if (ParseExtension(tsgn->Extension, &ExtInfo) != 0)
    ID = 0;

  if (ID >= 3 && ID < 16)
  {
    UA = ExtInfo.BT_or_UA;

    if (   *UA != 'b'
        || (   ExtInfo.CellChoice != ' '
            && ExtInfo.CellChoice != '1'))
    {
      if (*UA == '-')
      {
        ID += 3000;
        UA++;
      }

      switch (*UA)
      {
        case 'b': ID += 10000; break;
        case 'c': ID += 20000; break;
        case 'a': ID += 30000; break;
        default:  ID = 0;      break;
      }

      if (ID != 0)
      {
        switch (ExtInfo.CellChoice)
        {
          case ' ':             break;
          case '1': ID += 1000; break;
          case '2': ID += 2000; break;
          case '3': ID += 3000; break;
          default:  ID = 0;     break;
        }
      }
    }
  }
  else
  {
    if (ExtInfo.BasisChoice == 'R')
      ID += 20000;
    else
    {
      if (ExtInfo.BT_or_UA[0])
      {
        for (iBT = 0; iBT < 6; iBT++)
          if (ExtInfo.BT_or_UA == Ext_BT_or_UA[iBT])
            break;
      }
      else
        iBT = 0;

      if (iBT < 6)
      {
        if (ExtInfo.OriginChoice == '2') ID += 20000;
        else if (iBT)                    ID += 10000;

        if (iBT)
          ID += (iBT + 1) * 1000;
      }
      else
        ID = 0;
    }
  }

  if (ID == 0)
    SetSgError(IErr_Corrupt_TabSgName);

  return ID;
}


int ParseSymXYZ(const char *SymXYZ, T_RTMx *SeitzMx, int FacTr)
{
  unsigned int  P_mode;
  int           Row, Column, Sign, GotXYZ, i;
  double        Value, Value1, Value2, Delta;


  for (i = 0; i < 12; i++) SeitzMx->a[i] = 0;

#define P_Blank   0x01u
#define P_Comma   0x02u
#define P_Plus    0x04u
#define P_Dash    0x08u
#define P_Slash   0x10u
#define P_Value1  0x20u
#define P_Value2  0x40u
#define P_XYZ     0x80u

  Value1 = 0.;

  Row    = 0;
  Sign   = 1;
  Value  = 0.;
  GotXYZ = 0;
  P_mode = P_Blank | P_Plus | P_Dash | P_Value1 | P_XYZ;

  do
  {
    switch (*SymXYZ)
    {
      case ' ':
      case '\t':
      case '_':
        if ((P_mode & P_Blank) == 0) return -1;
        break;
      case ',':
      case ';':
        if (Row == 2)                return -1;
      case '\0':
        if ((P_mode & P_Comma) == 0) return -1;
        if (GotXYZ == 0)             return -1;
        if (P_mode & P_Slash) Value += Value1;
        Value *= FacTr;
        if (Value < 0.) i = (int)(Value - .5);
        else            i = (int)(Value + .5);
        Delta = Value - i;
        if (Delta < 0.) Delta = -Delta;
        if (Delta > .01 * FacTr) return -1;
        i %= FacTr; if (i < 0) i += FacTr;
        SeitzMx->s.T[Row] = i;
        Row++;
        Sign   = 1;
        Value  = 0.;
        P_mode = P_Blank | P_Plus | P_Dash | P_Value1 | P_XYZ;
        GotXYZ = 0;
        break;
      case '+':
        if ((P_mode & P_Plus)  == 0) return -1;
        if (P_mode & P_Slash) Value += Value1;
        Sign =  1;
        if (P_mode & P_Value2)
          P_mode = P_Value2;
        else
          P_mode = P_Blank | P_Value1 | P_XYZ;
        break;
      case '-':
        if ((P_mode & P_Dash)  == 0) return -1;
        if (P_mode & P_Slash) Value += Value1;
        Sign = -1;
        if (P_mode & P_Value2)
          P_mode = P_Value2;
        else
          P_mode = P_Blank | P_Value1 | P_XYZ;
        break;
      case '/':
      case ':':
        if ((P_mode & P_Slash) == 0) return -1;
        Sign =  1;
        P_mode = P_Blank | P_Plus | P_Dash | P_Value2;
        break;
      case '.':
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        if      (P_mode & P_Value1)
        {
          if (sscanf(SymXYZ, "%lf%n", &Value1, &i) != 1) return -1;
          if (Sign == -1) Value1 = -Value1;
          P_mode = P_Blank | P_Comma | P_Plus | P_Dash | P_Slash;
        }
        else if (P_mode & P_Value2)
        {
          if (sscanf(SymXYZ, "%lf%n", &Value2, &i) != 1) return -1;
          if (Sign == -1) Value2 = -Value2;
          if (Value1 != 0.)
          {
            if (Value2 == 0.) return -1;
            Value += Value1 / Value2;
          }
          P_mode = P_Blank | P_Comma | P_Plus | P_Dash;
        }
        else
          return -1;
        SymXYZ += (i - 1);
        break;
      case 'X':
      case 'x': Column = 0; goto Process_XYZ;
      case 'Y':
      case 'y': Column = 1; goto Process_XYZ;
      case 'Z':
      case 'z': Column = 2;
       Process_XYZ:
        if ((P_mode & P_XYZ) == 0) return -1;
        i = Row * 3 + Column;
        if (SeitzMx->s.R[i] != 0) return -1;
        SeitzMx->s.R[i] = Sign;
        GotXYZ = 1;
        P_mode = P_Blank | P_Comma | P_Plus | P_Dash;
        break;
    }
  }
  while (*SymXYZ++);

  if (Row != 3) return -1;

  return 0;

#undef P_Blank
#undef P_Comma
#undef P_Plus
#undef P_Dash
#undef P_Slash
#undef P_Value1
#undef P_Value2
#undef P_XYZ
}


static int LookupRotMx(T_HallGenerator *HG)
{
  int                   i, f, refaxis, dircode;
  int                   iNextBasis, nNextBasis;
  const T_TabXtalRotMx  *txrmx;


  if (HG->Rotation <= 0) return 0;

  refaxis = HG->RefAxis;
  dircode = HG->DirCode;

  if (HG->Rotation == 1)
  {
    refaxis = 'o';
    dircode = '.';
    nNextBasis = 0;
  }
  else if (dircode == '*')
  {
    if (refaxis == 0) refaxis = 'o';
    nNextBasis = 0;
  }
  else
  {
    if (dircode == 0) dircode = '=';

    switch (refaxis)
    {
      case 'z': nNextBasis = 0; break;
      case 'x': nNextBasis = 1; break;
      case 'y': nNextBasis = 2; break;
      default:
        return 0;
    }
  }

  for (txrmx = TabXtalRotMx; txrmx->Order; txrmx++)
    if (txrmx->Order == HG->Rotation) break;

  while (txrmx->Order == HG->Rotation)
  {
    if (txrmx->DirCode == dircode)
    {
      if (HG->Improper == 0) f =  1;
      else                   f = -1;

      for (i = 0; i < 9; i++)
        HG->SeitzMx.s.R[i] = txrmx->RMx[i] * f;

      for (iNextBasis = 0; iNextBasis < nNextBasis; iNextBasis++)
        RotateRotMx(HG->SeitzMx.s.R, RMx_3_111, RMx_3i111);

      return 1;
    }

    txrmx++;
  }

  return 0;
}


int ParseHallSymbol(const char *hsym, T_SgInfo *SgInfo)
{
  int                  c, i, pos_hsym;
  const int            *ht;
  int                  Centric;
  const T_LatticeInfo  *LatticeInfo;
  int                  FieldType, PreviousFT;
  int                  iOriginShift, SignOriginShift;
  int                  digit, rotation, refaxis, dircode;
  const int            *translation;
  int                  PreviousRotation, PreviousRefAxis;
  int                  nHG, ClearHG;
  T_HallGenerator      HG;

  enum ListOfFieldTypes
    {
      FT_Delimiter,
      FT_Improper,
      FT_Digit,
      FT_Rotation,
      FT_RefAxis,
      FT_DirCode,
      FT_Translation,
      FT_OriginShift
    };

  static const char *Err_Ill_ori_shi_val =
    "Error: Illegal origin shift value";

  static const char *Err_Too_ori_shi_val =
    "Error: Too much origin shift values";


  Centric = 0;
  LatticeInfo = NULL;

  HG.Rotation = HG.RefAxis = HG.DirCode = HG.Screw = 0;

  nHG = 0;
  ClearHG = 1;
  FieldType = FT_Delimiter;
  PreviousRotation = 0;
  PreviousRefAxis = 0;
  iOriginShift = 0;
  SignOriginShift = 0;

  pos_hsym = 0;

  do
  {
    if (*hsym == '_' || *hsym == '.' || *hsym == '\t' || *hsym == '\0')
      c = ' ';
    else
      c = *hsym;

    pos_hsym++;

    if (LatticeInfo == NULL)
    {
      if (Centric == 0 && c == '-')
      {
        if (AddInversion2ListSeitzMx(SgInfo) < 0)
          return pos_hsym;
        Centric = 1;
      }
      else if (c != ' ')
      {
        c = toupper(c);

        switch (c)
        {
          case 'P': LatticeInfo = LI_P; break;
          case 'A': LatticeInfo = LI_A; break;
          case 'B': LatticeInfo = LI_B; break;
          case 'C': LatticeInfo = LI_C; break;
          case 'I': LatticeInfo = LI_I; break;
          case 'R': LatticeInfo = LI_R; break;
          case 'S': LatticeInfo = LI_S; break;
          case 'T': LatticeInfo = LI_T; break;
          case 'F': LatticeInfo = LI_F; break;
          default:
            SetSgError("Error: Illegal lattice code");
            return pos_hsym;
        }

        if (AddLatticeTr2ListSeitzMx(SgInfo, LatticeInfo) < 0)
          return pos_hsym;
      }
    }
    else if (FieldType != FT_OriginShift)
    {
      c = tolower(c);
      if      (c == 'q') c = '\'';
      else if (c == '+') c = '"';

      PreviousFT = FieldType;
      digit = rotation = refaxis = dircode = 0;
      translation = NULL;

      ht = HallTranslations;

      while (*ht)
      {
        if (c == *ht)
        {
          translation = ht;
          FieldType = FT_Translation;
          break;
        }
        ht += 4;
      }

      if (translation == NULL)
      {
        switch (c)
        {
          case  ' ': FieldType = FT_Delimiter; break;

          case  '-': FieldType = FT_Improper; break;

          case  '1': digit = 1; FieldType = FT_Digit; break;
          case  '2': digit = 2; FieldType = FT_Digit; break;
          case  '3': digit = 3; FieldType = FT_Digit; break;
          case  '4': digit = 4; FieldType = FT_Digit; break;
          case  '5': digit = 5; FieldType = FT_Digit; break;
          case  '6': digit = 6; FieldType = FT_Digit; break;

          case  'x':
          case  'y':
          case  'z': refaxis = c; FieldType = FT_RefAxis; break;

          case  '"':
          case '\'':
          case  '*': dircode = c; FieldType = FT_DirCode; break;

          case  '(': FieldType = FT_OriginShift; break;

          default:
            SetSgError("Error: Illegal character in Hall symbol");
            return pos_hsym;
        }

        if (FieldType == FT_Digit)
        {
          if (   ClearHG == 0
              && HG.Rotation > digit
              && HG.Screw == 0
              && HG.DirCode == 0)
          {
            HG.Screw = digit;
            FieldType = FT_Translation;
          }
          else if (digit == 5)
          {
            SetSgError("Error: Illegal 5-fold rotation");
            return pos_hsym;
          }
          else
          {
            rotation = digit;
            FieldType = FT_Rotation;
          }
        }
      }

      if (   ClearHG == 0
          && (    FieldType == FT_Delimiter
              ||  FieldType == FT_OriginShift
              ||  FieldType  < PreviousFT
              || (FieldType == PreviousFT && FieldType != FT_Translation))
          && ! (   FieldType == FT_RefAxis && HG.RefAxis == 0
                && PreviousFT == FT_DirCode))
      {
        if (HG.RefAxis == 0)
        {
          if (nHG == 0)
            HG.RefAxis = 'z';
          else
          {
            if (HG.Rotation == 2)
            {
              if      (PreviousRotation == 2 || PreviousRotation == 4)
                HG.RefAxis = 'x';
              else if (PreviousRotation == 3 || PreviousRotation == 6)
              {
                HG.RefAxis = PreviousRefAxis;
                if (HG.DirCode == 0) HG.DirCode = '\'';
              }
            }
            else if (HG.Rotation == 3)
            {
              if (HG.DirCode == 0) HG.DirCode = '*';
            }
          }
        }

        PreviousRefAxis = HG.RefAxis;
        PreviousRotation = HG.Rotation;

        if (LookupRotMx(&HG) == 0)
        {
          SetSgError("Error: Illegal generator or need explicit axis symbol");
          return pos_hsym - 1;
        }

        if (HG.Screw)
        {
          switch (HG.RefAxis)
          {
            case 'x': i =  0; break;
            case 'y': i =  1; break;
            case 'z': i =  2; break;
            default:  i = -1; break;
          }

          if (HG.DirCode != 0 || i < 0)
          {
            SetSgError("Error: Screw for non-principal direction");
            return pos_hsym - 1;
          }

          HG.SeitzMx.s.T[i] += STBF * HG.Screw / HG.Rotation;
        }

        for (i = 0; i < 3; i++)
          HG.SeitzMx.s.T[i] %= STBF;

        if (Add2ListSeitzMx(SgInfo, &HG.SeitzMx) < 0)
          return pos_hsym - 1;

        if (SgInfo->StatusLatticeTr == -1)
        {
          if (AddLatticeTr2ListSeitzMx(SgInfo, SgInfo->LatticeInfo) < 0)
            return pos_hsym - 1;
        }

        nHG++;
        ClearHG = 1;
      }

      if (FieldType != FT_Delimiter && FieldType != FT_OriginShift)
      {
        if (ClearHG)
        {
          HG.Improper = 0;
          HG.Rotation = 1;
          HG.RefAxis = 0;
          HG.DirCode = 0;
          HG.Screw = 0;
          for (i = 0; i < 12; i++) HG.SeitzMx.a[i] = 0;

          ClearHG = 0;
        }

        switch (FieldType)
        {
          case FT_Improper:    HG.Improper = 1;        break;
          case FT_Rotation:    HG.Rotation = rotation; break;
          case FT_RefAxis:     HG.RefAxis  = refaxis;  break;
          case FT_DirCode:     HG.DirCode  = dircode;  break;
          case FT_Translation:
            if (translation != NULL)
            {
              for (i = 0; i < 3; i++)
                HG.SeitzMx.s.T[i] += *(++translation);
            }
            break;
        }
      }
    }
    else /* FieldType == FT_OriginShift */
    {
      if (iOriginShift > 3) {
        SetSgError(Err_Too_ori_shi_val);
        return pos_hsym;
      }

      if (*hsym == '\0') c = ')';

      digit = -1;

      switch (c)
      {
        case ' ': break;

        case ')':
          if (iOriginShift != 3)
          {
            SetSgError("Error: Missing origin shift values");
            return pos_hsym;
          }
          iOriginShift++;
          FieldType = FT_Delimiter;
          break;

        case '-':
          if (SignOriginShift != 0) {
            SetSgError(Err_Ill_ori_shi_val);
            return pos_hsym;
          }
          SignOriginShift = 1;
          break;

        case '0': digit = 0; break;
        case '1': digit = 1; break;
        case '2': digit = 2; break;
        case '3': digit = 3; break;
        case '4': digit = 4; break;
        case '5': digit = 5; break;
        case '6': digit = 6; break;

        default:
          SetSgError(Err_Ill_ori_shi_val);
          return pos_hsym;
      }

      if (digit >= 0)
      {
        if (iOriginShift >= 3) {
          SetSgError(Err_Too_ori_shi_val);
          return pos_hsym;
        }
        if (SignOriginShift) digit *= -1;
        SignOriginShift = 0;
        SgInfo->OriginShift[iOriginShift++] = digit;
      }
    }
  }
  while (*hsym++ != '\0');

  if (LatticeInfo == NULL) {
    SetSgError("Error: Lattice type not specified");
    return pos_hsym;
  }

  return pos_hsym;
}


static const char *PrintSgLabel(const char *lbl, int space, int *n,
                                FILE *fpout)
{
  while (*lbl && *lbl != ' ')
  {
    if (*lbl == '_')
    {
      if (space)
      {
        putc(space, fpout);
        if (n) (*n)++;
      }
    }
    else
    {
      putc(*lbl, fpout);
      if (n) (*n)++;
    }

    lbl++;
  }

  return lbl;
}


int PrintFullHM_SgName(const T_TabSgName *tsgn, int space, FILE *fpout)
{
  int         n;
  const char  *lbl;


  lbl = tsgn->SgLabels;

  if (tsgn->SgNumber >= 3 && tsgn->SgNumber < 16)
    while (*lbl) if (*lbl++ == '=') break;

  SkipWhite(lbl);

  n = 0;

  PrintSgLabel(lbl, space, &n, fpout);

  lbl = tsgn->Extension;

  if (*lbl && strchr("12HhRr", *lbl))
  {
    putc(':', fpout);
    putc(*lbl, fpout);
    n += 2;
  }

  return n;
}


void PrintTabSgNameEntry(const T_TabSgName *tsgn, int Style, int space,
                         FILE *fpout)
{
  int         n;
  const char  *lbl, *SfSymbol;


  if (Style)
    n = fprintf(fpout, "%3d", tsgn->SgNumber);
  else
    n = fprintf(fpout,  "%d", tsgn->SgNumber);

  if (tsgn->Extension[0])
    n += fprintf(fpout, ":%s", tsgn->Extension);

  if (Style)
    while (n < 9) { putc(' ', fpout); n++; }

  putc(' ', fpout); n++;
  putc(' ', fpout); n++;

  if (tsgn->SgNumber >= 1 && tsgn->SgNumber <= 230)
    SfSymbol = SchoenfliesSymbols[tsgn->SgNumber];
  else
    SfSymbol = "";

  n += fprintf(fpout, "%s", SfSymbol);

  if (Style)
    while (n < 23) { putc(' ', fpout); n++; }

  putc(' ', fpout); n++;
  putc(' ', fpout); n++;

  if (tsgn->SgNumber >= 3 && tsgn->SgNumber < 16)
  {
    lbl = PrintSgLabel(tsgn->SgLabels, space, &n, fpout);

    if (tsgn->Extension[0])
      n += fprintf(fpout, ":%s", tsgn->Extension);

    putc(' ', fpout); putc('=', fpout); putc(' ', fpout); n += 3;

    n += PrintFullHM_SgName(tsgn, space, fpout);

    while (*lbl) if (*lbl++ == '=') break;
    while (*lbl) if (*lbl++ == '=') break;
    SkipWhite(lbl);

    if (*lbl)
    {
      putc(' ', fpout); putc('=', fpout); putc(' ', fpout); n += 3;

      PrintSgLabel(lbl, space, &n, fpout);
    }
  }
  else
    n += PrintFullHM_SgName(tsgn, space, fpout);

  if (Style)
    while (n < 51) { putc(' ', fpout); n++; }

  putc(' ', fpout);
  putc(' ', fpout);

  fprintf(fpout, "%s", tsgn->HallSymbol);
}


static int FindGCD2(int ri, int rj)
{
  int  rk;


  if (ri < 0) ri = -ri;

  if (rj)
  {
    for (;;)
    {
      rk = ri % rj; if (rk == 0) { ri = rj; break; }
      ri = rj % rk; if (ri == 0) { ri = rk; break; }
      rj = rk % ri; if (rj == 0) {          break; }
    }

    if (ri < 0) ri = -ri;
  }

  return ri;
}


static void SimplifyFraction(int nume, int deno, int *o_nume, int *o_deno)
{
  int gcd = FindGCD2(nume, deno);
  if (gcd)
  {
    *o_nume = nume / gcd;
    *o_deno = deno / gcd;

    if (*o_deno < 0) {
      *o_nume *= -1;
      *o_deno *= -1;
    }
  }
}


const char *FormatFraction(int nume, int deno, int Decimal,
                           char *Buffer, int SizeBuffer)
{
  int          n=0;
  int          d=0;
  char         *cp, *cpp;
  static char  StaticBuffer[40];


  if (NULL == Buffer) {
              Buffer =        StaticBuffer;
          SizeBuffer = sizeof StaticBuffer / sizeof (*StaticBuffer);
  }

  Buffer[SizeBuffer - 1] = '\0';

  if (nume == 0)
  {
    Buffer[0] = '0';
    Buffer[1] = '\0';
  }
  if (Decimal)
  {
    sprintf(Buffer, "%.6g", (double) nume / deno);

         cp = Buffer;
    if (*cp == '-') cp++;
    if (*cp == '0') {
      cpp = cp + 1; while (*cp) *cp++ = *cpp++;
    }
  }
  else
  {
    SimplifyFraction(nume, deno, &n, &d);

    if (d == 1)
      sprintf(Buffer, "%d", n);
    else
      sprintf(Buffer, "%d/%d", n, d);
  }

  if (Buffer[SizeBuffer - 1] != '\0') {
      Buffer[SizeBuffer - 1] =  '\0';
    SetSgError("Internal Error: FormatFraction(): Buffer too small");
    return NULL;
  }

  return Buffer;
}


const char *RTMx2XYZ(const T_RTMx *RTMx, int FacRo, int FacTr,
                     int Decimal, int TrFirst, int Low,
                     const char *Seperator,
                     char *BufferXYZ, int SizeBufferXYZ)
{
  static const char *UpperXYZ = "XYZ";
  static const char *LowerXYZ = "xyz";

  int         i, j, p, iRo, iTr;
  char        *xyz, buf_tr[32];
  const char  *sep, *LetterXYZ, *ro, *tr;

  static char  StaticBufferXYZ[80];


  if (NULL == BufferXYZ) {
              BufferXYZ  =        StaticBufferXYZ;
          SizeBufferXYZ  = sizeof StaticBufferXYZ / sizeof (*StaticBufferXYZ);
  }

  BufferXYZ[SizeBufferXYZ - 1] = '\0';

  if (Low)
    LetterXYZ = LowerXYZ;
  else
    LetterXYZ = UpperXYZ;

  if (Seperator == NULL)
      Seperator = ",";

  xyz = BufferXYZ;

  for (i = 0; i < 3; i++)
  {
    if (i != 0)
      for (sep = Seperator; *sep; sep++) *xyz++ = *sep;

        iTr = iModPositive(RTMx->s.T[i], FacTr);
    if (iTr >  FacTr / 2)
        iTr -= FacTr;

        tr = FormatFraction(iTr, FacTr, Decimal,
                            buf_tr, sizeof buf_tr / sizeof (*buf_tr));
    if (tr == NULL)
      return NULL;

    p = 0;

    if (  TrFirst && iTr) {
      if (*tr) p = 1;
      while (*tr) *xyz++ = *tr++;
    }

    for (j = 0; j < 3; j++)
    {
          iRo = RTMx->s.R[i * 3 + j];
      if (iRo)
      {
            ro = FormatFraction(iRo, FacRo, Decimal, NULL, 0);
        if (ro == NULL)
          return NULL;

        if      (*ro == '-')
          *xyz++ = *ro++;
        else if (*ro && p)
          *xyz++ = '+';

        if (ro[0] != '1' || ro[1] != '\0') {
          while (*ro) *xyz++ = *ro++;
          *xyz++ = '*';
        }

        *xyz++ = LetterXYZ[j];

        p = 1;
      }
    }

    if (! TrFirst && iTr)
    {
      if (*tr && *tr != '-' && p)
        *xyz++ = '+';

      while (*tr) *xyz++ = *tr++;
    }
  }

  *xyz = '\0';

  if (BufferXYZ[SizeBufferXYZ - 1] != '\0') {
      BufferXYZ[SizeBufferXYZ - 1] =  '\0';
    SetSgError("Internal Error: RTMx2XYZ(): BufferXYZ too small");
    return NULL;
  }

  return BufferXYZ;
}


void PrintMapleRTMx(const T_RTMx *RTMx, int FacRo, int FacTr,
                    const char *Label, FILE *fpout)
{
  int         i, j, nt;
  const int   *r, *t;
  const char  *ff;


  if (Label)
    fprintf(fpout, "%s", Label);

  fprintf(fpout, " := matrix(4,4, [");

  r = RTMx->s.R;
  t = RTMx->s.T;

  for (i = 0; i < 3; i++, t++)
  {
    putc(' ', fpout);

    for (j = 0; j < 3; j++, r++)
    {
          ff = FormatFraction(*r, FacRo, 0, NULL, 0);
      if (ff == NULL)
        return;

      fprintf(fpout, "%s,", ff);
    }

        nt = iModPositive(*t, FacTr);
    if (nt >  FacTr / 2)
        nt -= FacTr;

        ff = FormatFraction(nt, FacTr, 0, NULL, 0);
    if (ff == NULL)
      return;

    fprintf(fpout, "%s,", ff);
  }

  fprintf(fpout, " 0,0,0,1]);\n");
}


static void PrintSeitzMx(const T_RTMx *SMx, FILE *fpout)
{
  int         i, nt;
  const char  *ff;
  const int   *r, *t;


  r = SMx->s.R;
  t = SMx->s.T;

  for (i = 0; i < 3; i++)
  {
    fprintf(fpout, " %2d", *r++);
    fprintf(fpout, " %2d", *r++);
    fprintf(fpout, " %2d", *r++);

        nt = iModPositive(*t++, STBF);
    if (nt >  STBF / 2)
        nt -= STBF;

        ff = FormatFraction(nt, STBF, 0, NULL, 0);
    if (ff == NULL)
      return;

    fprintf(fpout, " %6s\n", ff);
  }

  putc('\n', fpout);
}


void ListSgInfo(const T_SgInfo *SgInfo, int F_XYZ, int F_Verbose, FILE *fpout)
{
  int           iList, i_si_v;
  char          buf[8];
  const char    *xyz;
  const T_RTMx  *lsmx;
  T_RotMxInfo   *rmxi, RotMxInfo;


  iList = PG_Index(SgInfo->PointGroup);

  fprintf(fpout, "Point Group  %s\n", PG_Names[iList]);
  fprintf(fpout, "Laue  Group  %s\n",
    PG_Names[PG_Index(LG_Code_of_PG_Index[iList])]);

  fprintf(fpout, "%s\n", XS_Name[SgInfo->XtalSystem]);

  if (SgInfo->UniqueRefAxis != 0 || SgInfo->UniqueDirCode != 0)
  {
    fprintf(fpout, "Unique Axis  ");
    if (SgInfo->UniqueRefAxis != 0 && SgInfo->UniqueRefAxis != 'o')
      fprintf(fpout, "%c", SgInfo->UniqueRefAxis);
    if (SgInfo->UniqueDirCode != 0 && SgInfo->UniqueDirCode != '=')
      fprintf(fpout, "%c", SgInfo->UniqueDirCode);
    fprintf(fpout, "\n");
  }

  if (SgInfo->ExtraInfo != EI_Unknown)
    fprintf(fpout, "%s\n", EI_Name[SgInfo->ExtraInfo]);

  if (SgInfo->InversionOffOrigin)
    fprintf(fpout, "Note: Inversion operation off origin\n");

  putc('\n', fpout);

  fprintf(fpout, "Order   %3d\n", SgInfo->OrderL);
  fprintf(fpout, "Order P %3d\n", SgInfo->OrderP);
  putc('\n', fpout);

  if (SgInfo->n_si_Vector >= 0)
  {
    fprintf(fpout, "s.i.Vector  Modulus\n");
    for (i_si_v = 0; i_si_v < SgInfo->n_si_Vector; i_si_v++)
      fprintf(fpout, " %2d %2d %2d   %d\n",
        SgInfo->si_Vector[i_si_v * 3 + 0],
        SgInfo->si_Vector[i_si_v * 3 + 1],
        SgInfo->si_Vector[i_si_v * 3 + 2],
        SgInfo->si_Modulus[i_si_v]);
    putc('\n', fpout);
  }

  if (F_XYZ || F_Verbose)
  {
    fprintf(fpout, "#List   %3d\n", SgInfo->nList);
    putc('\n', fpout);

    lsmx = SgInfo->ListSeitzMx;
    rmxi = SgInfo->ListRotMxInfo;

    if (rmxi == NULL) rmxi = &RotMxInfo;

    for (iList = 0; iList < SgInfo->nList; iList++, lsmx++)
    {
      if (rmxi == &RotMxInfo)
      {
        if (GetRotMxInfo(lsmx->s.R, &RotMxInfo) == 0) {
          SetSgError("Error: Illegal SeitzMx in list");
          return;
        }
      }

      if (F_Verbose)
      {
        sprintf(buf, "(%d)", iList + 1);
        fprintf(fpout, "%-4s", buf);

        fprintf(fpout, "  %2d", rmxi->Order);
        if (rmxi->Inverse) fprintf(fpout, "^-1");
        else               fprintf(fpout, "   ");

        fprintf(fpout, " [%2d %2d %2d]",
                        rmxi->EigenVector[0],
                        rmxi->EigenVector[1],
                        rmxi->EigenVector[2]);

        if (rmxi->RefAxis) fprintf(fpout, " '%c'", rmxi->RefAxis);
        else               fprintf(fpout, "    ");
        if (rmxi->DirCode) fprintf(fpout, " '%c'", rmxi->DirCode);
        else               fprintf(fpout, "    ");

        fprintf(fpout, "    ");
      }

          xyz = RTMx2XYZ(lsmx, 1, STBF, 0, 0, 1, ", ", NULL, 0);
      if (xyz)
        fprintf(fpout, "%s", xyz);

      putc('\n', fpout);

      if (xyz == NULL)
        return;

      if (F_Verbose)
        PrintSeitzMx(lsmx, fpout);

      if (rmxi != &RotMxInfo) rmxi++;
    }

    if (iList && F_Verbose == 0)
      putc('\n', fpout);
  }
}
