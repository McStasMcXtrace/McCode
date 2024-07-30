/********************************************************************************************/
/*  VITESS module 'general.c'                                                               */
/*    Elementary functions for all VITESS modules                                           */
/*                                                                                          */
/* The free non-commercial use of these routines is granted providing due credit is given   */
/* to the authors:                                                                          */
/* Friedrich Streffer, Géza Zsigmond, Dietmar Wechsler,                                     */
/* Michael Fromme, Klaus Lieutenant, Sergey Manoshin                                        */ 
/*                                                                                          */
/* Change: K.L.  2002 JAN, reorganized routines                                             */
/* Change: G.Zs. 2002 JUL, new routines                                                     */
/* Change: K.L.  2003 JAN, new functions 'ReadLine', 'StrgLShift', and 'StrgCopy'           */
/* Change: K.L.  2003 FEB, definitions of 'idum' and 'LogFilePtr' from init to general      */
/* Change: K.L.  2003 MAR, new function 'StrgScanLF', additional parameter in 'ReadLine'    */

#ifdef VITESS
 #include "general.h"
#else
 %include "general.h"
#endif
#include "ctype.h"


#define MBIG 1000000000
#define MSEED 161803398
#define MZ 0
#define FAC (1.0/(double)MBIG)

long  idum;              /* parameter for the random number generation  */
FILE* LogFilePtr;        /* pointer to the log file stream              */


/****************************************************************************************/
/*  Coversion between physical properties                                               */
/****************************************************************************************/

double ENERGY_FROM_LAMBDA(double x) 
{	
	return(81805.048 / x / x); /*[ueV]*/
}

double LAMBDA_FROM_ENERGY(double x)
{
	return(sqrt(81805.048 / x));  /*[ueV]*/
}

double ENERGY_FROM_V(double x) 
{
	return(0.5227032667573 * x * x); /*[ueV]*/
}

double V_FROM_LAMBDA(double x)
{
	return(395.60346 / x); /* cm/ms 395.6034613488 */
}
double LAMBDA_FROM_V(double x)
{
	return(395.60346 / x); /* cm/ms 395.6034613488 */
}


/****************************************************************************************/
/*  Random Functions                                                                    */
/****************************************************************************************/

/* 'ran3' computes next random variable in [0,1[         */
/* (C) Copr. 1986-92 Numerical Recipes Software #>,1'59. */

double ran3(long *idum)
{
	static int inext,inextp;
	static long ma[56];
	static int iff=0;
	long mj,mk;
	int i,ii,k;

	if (*idum < 0 || iff == 0) {
		iff=1;
		mj=MSEED-(*idum < 0 ? -*idum : *idum);
		mj %= MBIG;
		ma[55]=mj;
		mk=1;
		for (i=1;i<=54;i++) {
			ii=(21*i) % 55;
			ma[ii]=mk;
			mk=mj-mk;
			while (mk < MZ) mk += MBIG;
			mj=ma[ii];
		}
		for (k=1;k<=4;k++)
			for (i=1;i<=55;i++) {
				ma[i] -= ma[1+(i+30) % 55];
				 while (ma[i] < MZ) ma[i] += MBIG;
			}
		inext=0;
		inextp=31;
		*idum=1;
	}
	if (++inext == 56) inext=1;
	if (++inextp == 56) inextp=1;
	mj=ma[inext]-ma[inextp];
	while (mj < MZ) mj += MBIG;  /*modification */
	ma[inext]=mj;
	return FAC*(double)mj;
}
#undef MBIG
#undef MSEED
#undef MZ
#undef FAC

double MonteCarlo(double x, double y)
{
   return((y-x)*ran3(&idum) + x);
}

/* 'vector3rand' simulates randomly orientied vector with unit lenght */
/*  (Manoshin Sergey 28.02.01)                                        */

double vector3rand(double *vx, double *vy, double *vz)
{
  double len;
  len = 0.0;

  while ((len == 0.0)||(len > 1.0))
    {
      *vx = 2.0*ran3(&idum) - 1.0;
      *vy = 2.0*ran3(&idum) - 1.0;
      *vz = 2.0*ran3(&idum) - 1.0;
      len = (*vx)*(*vx) + (*vy)*(*vy) + (*vz)*(*vz);
    }

  /*     Normalising  */
  len = sqrt(len);

  *vx = *vx/len;
  *vy = *vy/len;
  *vz = *vz/len;

  return(len);
}


/****************************************************************************************/
/*  General Functions                                                                   */
/****************************************************************************************/

/* computes square of a real value */

double sq(double Value)
{
	return Value * Value ;
}



/* calculates atan2 in the range (0, 2*M_PI) */

double atan0(double a, double b)
{
	if (b > 0.)		return (double) atan(a / b) ;

	if (b == 0.)	return M_PI_2 ;

	else			return (double) atan(a / b) + M_PI ;
}



/* minimum and maximum of two double or long values */

long mini(long value1, long value2)
{
	if(value1 < value2) return value1 ;
	else                return value2 ;
}

long maxi(long value1, long value2)
{
	if(value1 > value2) return value1 ;
	else                return value2 ;
}

double Min(double value1, double value2)
{
	if(value1 < value2) return value1 ;
	else                return value2 ;
}

double Max(double value1, double value2)
{
	if(value1 > value2) return value1 ;
	else                return value2 ;
}



/* 'Exchange' of two values */

void Exchange(double* pValue1, double* pValue2)
{
	double dHelp;

	dHelp    = *pValue1 ;
	*pValue1 = *pValue2;
	*pValue2 = dHelp;

	return;
}



/****************************************************************************************/
/*  Vector Functions                                                                    */
/****************************************************************************************/

/* 'Copy' copies the contents of Vector 'Src' to vector 'Dest'  */
/*                                                    */
void CopyVector(const VectorType Src, VectorType Dest)
{
	int i;

	for(i=0;i<3;i++)
	{	Dest[i] = Src[i];
	}
}


/* 'MAXV' returns the number of the largest component of 'Vector': 0, 1 or 2  */
/*                                                                            */
long MAXV(const VectorType Vector)
{
	if( (fabs(Vector[0]) > fabs(Vector[1])) && (fabs(Vector[0]) > fabs(Vector[2])))
		return 0;
	else
		if(fabs(Vector[1]) > fabs(Vector[2]))
			return 1;
		else
			return 2;
}


/* 'LengthVector' returns the length of vector 'Vec'  */
/*                                                    */
double LengthVector(const VectorType Vec)
{
	return sqrt(ScalarProduct(Vec,Vec));
}


/* 'NormVector' changes the vector length to 1  */
/*                                              */
short NormVector(VectorType Vector)
{
	long   i;
	double dLen = LengthVector(Vector);	

	if (dLen==0.0)
	{	return FALSE;
	}
	else
	{	for(i=0;i<3;i++)
		{	Vector[i] /= dLen;
		}
		return TRUE;
	}
}


/* 'DistVector' calculates the distance between the points described by Vec1 and Vec2  */
/*                                                                                     */
double DistVector(const VectorType Vec1, const VectorType Vec2)
{
	VectorType Vhlp;

	CopyVector(Vec1, Vhlp) ;
	SubVector (Vhlp, Vec2);
	return LengthVector(Vhlp);
}


/* 'AddVector' adds 'Add' to 'Value' and returns 'Value'  */
/*                                                        */
void AddVector(VectorType Value, const VectorType Add)
{
	int i ;
	VectorType Result ;
	for(i=0;i<3;i++)
		Result[i]=Value[i]+ Add[i] ;
	CopyVector(Result, Value) ;
}


/* 'SubVector' Substracts 'Sub' to 'Value' and returns 'Value' */
/*                                                             */
void SubVector(VectorType Value, const VectorType Sub)
{
	int i ;
	VectorType Result ;
	for(i=0;i<3;i++)
	{
		Result[i]=Value[i]- Sub[i] ;
		Value[i] = Result[i] ;
	}
}


/* 'MultiplyByScalar' multiplies a vector by a scalar */
/*                                                    */
void MultiplyByScalar(VectorType Vector, const double Scalar)
{
	int i ;
	VectorType Result ;
	for(i=0;i<3;i++)
	{
		Result[i] = Scalar * Vector[i] ;
		Vector[i] = Result[i] ;
	}
}


/* 'ScalarProduct' calculates the scalar product of two vectors 'v1' and 'v2' */
/*                                                                            */
double ScalarProduct(const VectorType v1, const VectorType v2)
{
	int		j;
	double	result;

	result = 0.;
	for(j=0;j<3;j++) result += v1[j]*v2[j] ;

	return result ;
}

/* angle between two vectors in degs */

double AngleVectors(VectorType v1, VectorType v2)
{
double theta ;
 
	theta= ScalarProduct(v1, v2) / (double)sqrt(ScalarProduct(v1, v1)) / (double)sqrt(ScalarProduct(v2, v2)) ;
	return 180./M_PI * (double) acos(theta) ;
}

/* area of triangle from two vectors, G.Zs */


double Area(VectorType v1, VectorType v2)
{
return LengthVector(v1) * LengthVector(v2) * 
		
		fabs(sin(acos( ScalarProduct(v1, v2)/(LengthVector(v1) * LengthVector(v2)))) /2.);

}


/* 'RotVector' does essentially a Vector times matrix multiplication   */
/* in order to rotate the Vector. The rotation Matrix may be supplied  */
/* by e.g. 'RotMatrixX'                                                */
/* Author: F. Streffer.                                                */
void RotVector(double RotMatrix[3][3], VectorType Vector)
{
	VectorType TempVec;
	int        i;

	for(i=0;i<3;i++)
		TempVec[i]=ScalarProduct(RotMatrix[i],Vector);
	CopyVector(TempVec, Vector);
}

/* 'RotBackVector' rotates a vector, by multiplication of the Vector  */
/* with the invers of RotMatrix. E.g if RotMatrix is the same as in   */
/* 'RotVector' and is applied to the result of RotVector the original */
/* vector is restored.   (Remember det(RotMatrix)=1)                  */
/* Author: F. Streffer.                                               */
void RotBackVector(double RotMatrix[3][3], VectorType Vector)
{
	VectorType TempVec;
	int        i;

	for(i=0;i<3;i++)
		TempVec[i]=RotMatrix[0][i]*Vector[0]+RotMatrix[1][i]*Vector[1]+RotMatrix[2][i]*Vector[2];
	CopyVector(TempVec, Vector);
}


/* 'FillRMatrixZY' calculates a rotation matrix, which rotates a frame   */
/* at first about the z-axis by 'rotz' and then about the y-axis by 'roty' */
/*  Author: F. Streffer.                                                   */
/*  Change: G. Zs. 16 JUL 2002  rotation convention                        */
void FillRMatrixZY(double RotMatrix[3][3], double roty, double rotz)
{
  double sz, cz, sy, cy;
  long   i,j;

  sy= (double) sin(roty);
  cy= (double) cos(roty);
  sz= (double) sin(rotz);
  cz= (double) cos(rotz);

  /* now, fill the matrix */
  RotMatrix[0][0] =  cy*cz;
  RotMatrix[0][1] =  cy*sz;
  RotMatrix[0][2] =  sy;
  RotMatrix[1][0] = -sz;
  RotMatrix[1][1] =  cz;
  RotMatrix[1][2] =  0.0;
  RotMatrix[2][0] = -sy*cz;
  RotMatrix[2][1] = -sy*sz;
  RotMatrix[2][2] =  cy;

  /* cutoff very small matrix elements */
  for(i=0; i<3; i++)
    for(j=0; j<3; j++)
      if(fabs(RotMatrix[i][j]) < 1e-12) RotMatrix[i][j] = 0.0;
}


/****************************************************************************************/
/*  General I/O Functions                                                               */
/****************************************************************************************/

/* fileOpen open file 'name' and gives pointer back 
   in case of an opening error, a message is written to the LogFile */

FILE * fileOpen(const char *name, char *mode)
{	
	FILE *f=NULL;
	
	f = fopen(name, mode);
	if (f==NULL) 
	{	fprintf(LogFilePtr, "ERROR: Can't open %s!\n", name);
		exit(-1);
	}
	return f;
}


void Error(const char *text)
{
	fprintf(LogFilePtr,"ERROR: %s!\n", text);
	exit(-1);
}


void Warning(const char *text)
{
	fprintf(LogFilePtr,"Warning: %s!\n", text);
}


void Abort()
{
	exit(-1);
}



/****************************************************************************************/
/*  Functions for Reading of Input Data                                                 */
/****************************************************************************************/

/* ReadLine reads next line from file 'pFile' into string 'pLine' that is 
     not empty and not a comment line (beginning with #)
	  returning TRUE if line is found and FALSE otherwise
   it strips comments at the end, leading and succeeding blanks, line feeds, tabs anc cr 
   the maximal number of characters in the string must be given in 'nStrLen'
*/
int
ReadLine(FILE* pFile, char* pLine, int nStrLen)
{
	char *pComment;
	short k, kmax;

	strcpy(pLine, "");
	if (pFile!=NULL)
	{
		while(strlen(pLine)==0  && !feof(pFile))	
		{	
			fgets (pLine, nStrLen, pFile);

			/* delete line feeds, tabs and carriage returns */
			kmax = (short) strlen(pLine);
			for (k=0; k < kmax; k++)
			{	if (pLine[k]=='\n' || pLine[k]=='\t' || pLine[k]=='\r')
					pLine[k]=' ';
			}
			/* strip the comments and leading and succeeding blanks */
			pComment = strchr(pLine, '#');
			if (pComment != NULL)
				*pComment = '\0';
			while (pLine[0]==' ')
			{	StrgLShift(pLine,1);
			}
			while (pLine[strlen(pLine)-1]==' ')
			{	pLine[strlen(pLine)-1]='\0';
			}
		}
	}
	if (strlen(pLine) > 0)
		return TRUE;
	else
		return FALSE;
}


/*  ReadParString(FILE *fpt) reads one string value from parameter file */

void ReadParString(FILE *fpt, char *stringvar)
{
	fscanf(fpt,"%s", stringvar ) ;

	return ;
}


/*  ReadParF(FILE *fpt) reads one double value from parameter file */

double ReadParF(FILE *fpt)
{
	double value ;
	value=0. ;
	fscanf(fpt,"%lf", &value ) ;
	return value;
}


/*  ReadParI(FILE *fpt) reads one integer value from parameter file */

int ReadParI(FILE *fpt)
{
	int value ;
	value=0 ;
	fscanf(fpt,"%d", &value ) ;
	return value;
}


/* ReadParComment(FILE *fpt) reads comment line */

void ReadParComment(FILE *fpt)
{
	char comment[100], *c ;
	c=fgets(comment, 100, fpt) ;
}


/**********************************************************/
/*  String Operations                                     */
/**********************************************************/

/* Copy 'nLen' bytes of 'sOrigin' into the new string 'sCopy' */
void
StrgCopy(char* sCopy, const char* sOrigin, int nLen)
{
	strncpy(sCopy, sOrigin, nLen);
	sCopy[nLen]='\0';
}


/* Shift string 'sStr' 'kWidth' bytes to the left */
void
StrgLShift(char* sStr, int kWidth)
{
	int k, ke;

	ke = strlen(sStr) - kWidth;

	for (k=0; k <= ke; k++)
		sStr[k] = sStr[k+kWidth];
}


/* Scan string 'sStr' and copy all values (but maximally 'nMax') 
   to list 'pTab' of double values,  beginning with value number 'nStart'*/
long
StrgScanLF(const char* sStr, double* pTab, const int nMax, const int nStart)
{
	int    k, n=0;
	char   *pStr, sNumber[31];

	pStr = (char*) sStr;
	n   -= nStart;
	do
	{	/* search of beginning and end of 1st number of (remaining) string */
		k=0;
		/* step forward until first number or control character */
		while (isdigit(pStr[k])==0 && iscntrl(pStr[k])==0) 
			k++; 
		/* step forward until space-like or control character */
		while (isspace(pStr[k])==0 && iscntrl(pStr[k])==0) 
			k++;  

		/* separating first number and adding it to the list */
		if (k > 0)
		{	
			StrgCopy(sNumber, pStr, k);
			if (n >= 0)
				pTab[n] = atof(sNumber);
			n++;	
			pStr += k;
		}
	}
	while (n < nMax && k > 0);

	return(n);
}

