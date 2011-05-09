/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2008, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/interoff-lib.c
*
* %Identification
* Written by: Reynald Arnerin
* Date:    Jun 12, 2008
* Origin: ILL
* Release: $Revision: 1.8 $
* Version: McStas X.Y
*
* Object File Format intersection library for McStas. Requires the qsort function.
*
* Such files may be obtained with e.g.
*   qhull < points.xyz Qx Qv Tv o > points.off
* where points.xyz has format (it supports comments):
*   3
*   <nb_points>
*   <x> <y> <z>
*   ...
* The resulting file should have its first line being changed from '3' into 'OFF'.
* It can then be displayed with geomview.
* A similar, but somewhat older solution is to use 'powercrust' with e.g.
*   powercrust -i points.xyz
* which will generate a 'pc.off' file to be renamed as suited.
*
*******************************************************************************/

#ifndef INTEROFF_LIB_H
#include "interoff-lib.h"
#endif

double off_F(double x, double y,double z,double A,double B,double C,double D) {
  return ( A*x + B*y + C*z + D );
}

char off_sign(double a) { 
  if (a<0)       return(-1);
  else if (a==0) return(0);
  else           return(1);
}

// off_normal ******************************************************************
//gives the normal vector of p
void off_normal(Coords* n, polygon p)
{
  //using Newell method  
  int i=0,j=0;
  n->x=0;n->y=0;n->z=0;
  for (i = 0, j = p.npol-1; i < p.npol; j = i++)
  {
    MCNUM x1=p.p[3*i],
          y1=p.p[3*i+1], 
          z1=p.p[3*i+2];
    MCNUM x2=p.p[3*j],
          y2=p.p[3*j+1], 
          z2=p.p[3*j+2];
    // n is the cross product of v1*v2
    n->x += (y1 - y2) * (z1 + z2);
    n->y += (z1 - z2) * (x1 + x2);
    n->z += (x1 - x2) * (y1 + y2);
  }
} /* off_normal */

// off_pnpoly ******************************************************************
//based on http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
//return 0 if the vertex is out
//    1 if it is in
//   -1 if on the boundary
int off_pnpoly(polygon p, Coords v)
{
  int i=0, c = 0;
  MCNUM minx=FLT_MAX,maxx=-FLT_MAX,miny=FLT_MAX,maxy=-FLT_MAX,minz=FLT_MAX,maxz=-FLT_MAX;
  MCNUM rangex=0,rangey=0,rangez=0;

  int pol2dx=0,pol2dy=1;          //2d restriction of the poly
  MCNUM x=v.x,y=v.y;


  //take the most relevant 2D projection (prevent from instability)
  for (i=0; i<p.npol; ++i)
  {
    if (p.p[3*i]<minx)   minx=p.p[3*i];
    if (p.p[3*i]>maxx)   maxx=p.p[3*i];
    if (p.p[3*i+1]<miny) miny=p.p[3*i+1];
    if (p.p[3*i+1]>maxy) maxy=p.p[3*i+1];
    if (p.p[3*i+2]<minz) minz=p.p[3*i+2];
    if (p.p[3*i+2]>maxz) maxz=p.p[3*i+2];  
  }
  rangex=maxx-minx;
  rangey=maxy-miny;
  rangez=maxz-minz;

  if (rangex<rangez)
  {
    if (rangex<rangey) {
      pol2dx=2;    
      x=v.z;
    } else {
      pol2dy=2;
      y=v.z;
    }
  }
  else if (rangey<rangez) {
    pol2dy=2;
    y=v.z;    
  }

  //trace rays and test number of intersection
  int j;
  for (i = 0, j = p.npol-1; i < p.npol; j = i++) {
    if (((((p.p[3*i+pol2dy])<=y) && (y<(p.p[3*j+pol2dy]))) ||
         (((p.p[3*j+pol2dy])<=y) && (y<(p.p[3*i+pol2dy])))) &&
        (x < ( (p.p[3*j+pol2dx] - p.p[3*i+pol2dx]) * (y - p.p[3*i+pol2dy]) 
             / (p.p[3*j+pol2dy] - p.p[3*i+pol2dy]) + p.p[3*i+pol2dx]) ))  
      c = !c;

    if (((fabs(p.p[3*i+pol2dy]-y)<=EPSILON) || ((fabs(p.p[3*j+pol2dy]-y)<=EPSILON))) &&
        fabs(x -((p.p[3*j+pol2dx] - p.p[3*i+pol2dx]) * (y - p.p[3*i+pol2dy]) 
          / (p.p[3*j+pol2dy] - p.p[3*i+pol2dy]) + p.p[3*i+pol2dx])) < EPSILON)
    {
      //the point lies on the edge
      c=-1;
      break;
    }
  }

  return c;
} /* off_pnpoly */

// off_intersectPoly ***********************************************************
//gives the intersection vertex between ray [a,b) and polygon p and its parametric value on (a b)
//based on http://geometryalgorithms.com/Archive/algorithm_0105/algorithm_0105.htm
int off_intersectPoly(intersection *inter, Coords a, Coords b, polygon p)
{
  //direction vector of [a,b]
  Coords dir = {b.x-a.x, b.y-a.y, b.z-a.z};
  
  //the normal vector to the polygon
  Coords normale=p.normal;
  //off_normal(&normale, p); done at the init stage
  
  //direction vector from a to a vertex of the polygon
  Coords w0 = {a.x-p.p[0], a.y-p.p[1], a.z-p.p[2]};

  //scalar product
  MCNUM nw0  =-scalar_prod(normale.x,normale.y,normale.z,w0.x,w0.y,w0.z);
  MCNUM ndir = scalar_prod(normale.x,normale.y,normale.z,dir.x,dir.y,dir.z);
  inter->time = inter->edge = inter->in_out=0;
  inter->v = inter->normal = coords_set(0,0,1);

  if (fabs(ndir) < EPSILON)    // ray is parallel to polygon plane
  {     
    if (nw0 == 0)              // ray lies in polygon plane (infinite number of solution)
      return 0;
    else return 0;             // ray disjoint from plane (no solution)
  }

  // get intersect point of ray with polygon plane
  inter->time = nw0 / ndir;            //parametric value the point on line (a,b)

  inter->v = coords_set(a.x + inter->time * dir.x,// intersect point of ray and plane
    a.y + inter->time * dir.y,
    a.z + inter->time * dir.z);
        
  int res=off_pnpoly(p,inter->v);
  
  inter->edge=(res==-1);
  if (ndir<0)
    inter->in_out=1;  //the negative dot product means we enter the surface
  else
    inter->in_out=-1;
  
  inter->normal=p.normal;

  return res;         //true if the intersection point lies inside the poly
} /* off_intersectPoly */


// off_getBlocksIndex **********************************************************
/*reads the indexes at the beginning of the off file as this :
line 1  OFF
line 2  nbVertex nbFaces nbEdges
*/
long off_getBlocksIndex(char* filename, long* vtxIndex, long* vtxSize, long* faceIndex, long* polySize )
{
  if (!filename)             return(0);
  if (strlen(filename) == 0) return (0);
  if (!strcmp(filename,"NULL") || !strcmp(filename,"0"))  return(0);
  FILE* f = fopen(filename,"r");
  if (!f) {
    char mc_rt_path[CHAR_BUF_LENGTH];
    char mc_rt_dir[CHAR_BUF_LENGTH];

    if (!f)
    {
      strcpy(mc_rt_dir, getenv("MCSTAS") ? getenv("MCSTAS") : MCSTAS);
      sprintf(mc_rt_path, "%s%c%s%c%s", mc_rt_dir, MC_PATHSEP_C, "data", MC_PATHSEP_C, filename);
      f = fopen(mc_rt_path, "r");
    }
    if (!f)
    {
      strcpy(mc_rt_dir, getenv("MCSTAS") ? getenv("MCSTAS") : MCSTAS);
      sprintf(mc_rt_path, "%s%c%s%c%s", mc_rt_dir, MC_PATHSEP_C, "contrib", MC_PATHSEP_C, filename);
      f = fopen(mc_rt_path, "r");
    }
    if (!f)
    {
      fprintf(stderr, "Error: Could not open input file '%s' (interoff/off_getBlocksIndex)\n", filename);
      return (0);
    }
  }
  
  printf("Loading geometry file: %s\n", filename);
  char line[CHAR_BUF_LENGTH];
  char *ret=0;
  *vtxIndex = *vtxSize  = *faceIndex=0;
  ret=fgets(line,CHAR_BUF_LENGTH , f);// line 1 = "OFF"
  if (ret == NULL)
  {
    fprintf(stderr, "Error: Can not read 1st line in file %s (interoff/off_getBlocksIndex)\n", filename);
    exit(1);
  }

  if (strncmp(line,"OFF",3) && strncmp(line,"3",1))
  {
    fprintf(stderr, "Error: %s is probably not an OFF or NOFF file (interoff/off_getBlocksIndex).\n"
                    "       Requires first line to be 'OFF' or '3'.\n",filename);
    fclose(f);
    return(0);
  }

  *vtxIndex+= strlen(line);
        
  do  /* skip # comments which may be there */
  {
    ret=fgets(line,CHAR_BUF_LENGTH , f);
    if (ret == NULL)
    {
      fprintf(stderr, "Error: Can not read line in file %s (interoff/off_getBlocksIndex)\n", filename);
      exit(1);
    }
    *vtxIndex+= strlen(line);
  } while (line[0]=='#');
  
  //line = nblines of vertex,faces and edges arrays
  sscanf(line,"%lu %lu",vtxSize,polySize);

  *faceIndex=*vtxIndex;
  int i=0;
  for (i=0; i<*vtxSize; )
  {                              
    ret=fgets(line,CHAR_BUF_LENGTH,f);
    if (ret == NULL)
    {
      fprintf(stderr, "Error: Can not read vertex %i in file %s (interoff/off_getBlocksIndex)\n", i, filename);
      exit(1);
    }
    *faceIndex+=strlen(line); 
    if (line[0]!='#') i++;               
  }
 
  fclose(f);
  return(*vtxIndex);
} /* off_getBlocksIndex */

// off_init_planes *************************************************************
//gives the equations of 2 perpandicular planes of [ab]
void off_init_planes(Coords a, Coords b, 
  MCNUM* A1, MCNUM* C1, MCNUM* D1, MCNUM *A2, MCNUM* B2, MCNUM* C2, MCNUM* D2)
{
  //direction vector of [a b]  
  Coords dir={b.x-a.x, b.y-a.y, b.z-a.z};
  
  //the plane parallel to the 'y' is computed with the normal vector of the projection of [ab] on plane 'xz'  
  *A1= dir.z;
  *C1=-dir.x;
  if(*A1!=0 || *C1!=0)
    *D1=-(a.x)*(*A1)-(a.z)*(*C1);
  else
  {
    //the plane does not support the vector, take the one parallel to 'z''
    *A1=1;
    //B1=dir.x=0
    *D1=-(a.x);
  }
  //the plane parallel to the 'x' is computed with the normal vector of the projection of [ab] on plane 'yz'  
  *B2= dir.z;
  *C2=-dir.y;
  *A2= 0;
  if (*B2==0 && *C2==0)
  {
    //the plane does not support the vector, take the one parallel to 'z'
    *B2=1;
    //B1=dir.x=0
    *D2=-(a.y);
  }
  else {
    if (dir.z==0)
    {
      //the planes are the same, take the one parallel to 'z'
      *A2= dir.y;
      *B2=-dir.x;
      *D2=-(a.x)*(*A2)-(a.y)*(*B2); 
    }
    else
      *D2=-(a.y)**B2-(a.z)**C2;
  }
} /* off_init_planes */

// off_clip_3D_mod *************************************************************
int off_clip_3D_mod(intersection* t, Coords a, Coords b, 
  Coords* vtxArray, unsigned long vtxSize, unsigned long* faceArray, 
  unsigned long faceSize, Coords* normalArray)
{
  MCNUM A1=0, C1=0, D1=0, A2=0, B2=0, C2=0, D2=0;      //perpendicular plane equations to [a,b]
  off_init_planes(a, b, &A1, &C1, &D1, &A2, &B2, &C2, &D2);
  
  int t_size=0;
  //unsigned long vtxSize=vtxTable.rows, faceSize=faceTable.columns;  //Size of the corresponding tables
  char sg[vtxSize];  //array telling if vertex is left or right of the plane
  MCNUM popol[3*CHAR_BUF_LENGTH];
  unsigned long i=0,indPoly=0;
  for (i=0; i < vtxSize; ++i)
  {
    sg[i]=off_sign(off_F(vtxArray[i].x,vtxArray[i].y,vtxArray[i].z,A1,0,C1,D1));
  }
  
  //exploring the polygons :
  i=indPoly=0;
  while (i<faceSize)
  {
    polygon pol;
    pol.npol  = faceArray[i];                //nb vertex of polygon
    pol.p     = popol;
    pol.normal= coords_set(0,0,1);
    unsigned long indVertP1=faceArray[++i];  //polygon's first vertex index in vtxTable
    int j=1;
    while (j<pol.npol)
    {
      //polygon's j-th vertex index in vtxTable
      if (sg[indVertP1]!=sg[faceArray[i+j]]) //if the plane intersect the polygon
        break;  

      ++j;
    }
    
    if (j<pol.npol)          //ok, let's test with the second plane
    {
      char sg1=off_sign(off_F(vtxArray[indVertP1].x,vtxArray[indVertP1].y,vtxArray[indVertP1].z,A2,B2,C2,D2));//tells if vertex is left or right of the plane  
      
      j=1;
      while (j<pol.npol)
      {
        //unsigned long indVertPi=faceArray[i+j];  //polyg's j-th vertex index in vtxTable    
        Coords vertPi=vtxArray[faceArray[i+j]];
        if (sg1!=off_sign(off_F(vertPi.x,vertPi.y,vertPi.z,A2,B2,C2,D2)))//if the plane intersect the polygon
          break;
        ++j;
      }
      if (j<pol.npol)
      {
        if (t_size>CHAR_BUF_LENGTH)
        {
          fprintf(stderr, "Warning: number of intersection exceeded (%d) (interoff-lib/off_clip_3D_mod)\n", CHAR_BUF_LENGTH);
            return (t_size);
        }
        //both planes intersect the polygon, let's find the intersection point
        //our polygon :
        int k;
        for (k=0; k<pol.npol; ++k)
        {  
          Coords vertPk=vtxArray[faceArray[i+k]];
          pol.p[3*k]  =vertPk.x;
          pol.p[3*k+1]=vertPk.y;
          pol.p[3*k+2]=vertPk.z;
        }
        pol.normal=normalArray[indPoly];
        intersection x;
        if (off_intersectPoly(&x, a, b, pol))
        {            
          t[t_size++]=x;
        }          
      } /* if (j<pol.npol) */
    } /* if (j<pol.npol) */
    i += pol.npol;
    indPoly++;
  } /* while i<faceSize */
  return t_size;
} /* off_clip_3D_mod */


// off_compare *****************************************************************
int off_compare (void const *a, void const *b)
{
   intersection const *pa = a;
   intersection const *pb = b;

   return off_sign(pa->time - pb->time);
} /* off_compare */

// off_cleanDouble *************************************************************
//given an array of intesction throw those which appear several times
//returns 1 if there is a possibility of error
int off_cleanDouble(intersection* t, int* t_size)
{
  int i=1;
  intersection prev=t[0];
  while (i<*t_size)  
  {
    int j=i;
    //for each intersection with the same time
    while (j<*t_size && fabs(prev.time-t[j].time)<EPSILON)
    {
      //if the intersection is the exact same erase it
      if (prev.in_out==t[j].in_out)
      {
        int k;      
        for (k=j+1; k<*t_size; ++k)
        {
          t[k-1]=t[k];
        }
        *t_size-=1;  
      }
      else
        ++j;    
    }
    prev=t[i];      
    ++i;
        
  }
  return 1;
} /* off_cleanDouble */

// off_cleanInOut **************************************************************
//given an array of intesections throw those which enter and exit in the same time
//Meaning the ray passes very close to the volume
//returns 1 if there is a possibility of error
int off_cleanInOut(intersection* t, int* t_size)
{
  int i=1;
  intersection prev=t[0];
  while (i<*t_size)  
  {
    //if two intersection have the same time but one enters and the other exits erase both
    //(such intersections must be adjacent in the array : run off_cleanDouble before)
    if (fabs(prev.time-t[i].time)<EPSILON && prev.in_out!=t[i].in_out)
    {
      int j=0;    
      for (j=i+1; j<*t_size; ++j)
      {
        t[j-2]=t[j];
      }
      *t_size-=2;
      prev=t[i-1];      
    }
    else
    {
      prev=t[i];      
      ++i;
    }    
  }
  return (*t_size);
} /* off_cleanInOut */

/* PUBLIC functions ******************************************************** */

/*******************************************************************************
* long off_init(  char *offfile, double xwidth, double yheight, double zdepth, off_struct* data)
* ACTION: read an OFF file, optionally center object and rescale, initialize OFF data structure
* INPUT: 'offfile' OFF file to read
*        'xwidth,yheight,zdepth' if given as non-zero, apply bounding box. 
*           Specifying only one of these will also use the same ratio on all axes
*        'notcenter' center the object to the (0,0,0) position in local frame when set to zero
* RETURN: number of polyhedra and 'data' OFF structure 
*******************************************************************************/
long off_init(  char *offfile, double xwidth, double yheight, double zdepth, 
                int notcenter, off_struct* data)
{
  //data to be initialized
  long faceSize=0, vtxSize =0, polySize=0, vtxIndex=0, faceIndex=0;  
  Coords* vtxArray        =NULL;
  Coords* normalArray     =NULL;
  unsigned long* faceArray=NULL;
  t_Table vtxTable, faceTable;

  double minx=FLT_MAX,maxx=-FLT_MAX,miny=FLT_MAX,maxy=-FLT_MAX,minz=FLT_MAX,maxz=-FLT_MAX;

  // get the indexes
  if (!data || off_getBlocksIndex(offfile,&vtxIndex,&vtxSize,&faceIndex, &polySize) <=0) 
    return(0);
 
  //read vertex table = [x y z | x y z | ...]
  Table_Read_Offset(&vtxTable, offfile, 0, &vtxIndex, vtxSize);

  //read face table = [nbvertex v1 v2 vn nbvertex v1 v2 vn ...]
  Table_Read_Offset(&faceTable, offfile, 0, &faceIndex, 0);

  //initialize Arrays
  faceSize   = faceTable.columns;
  
  printf("  Number of polygons: %ld\n", polySize);
  printf("  Number of vertices: %ld\n", vtxSize);
  
  vtxArray   = malloc(vtxSize*sizeof(Coords));
  normalArray= malloc(polySize*sizeof(Coords));
  if (!vtxArray || !normalArray) return(0);

  long i=0,j=0;
  for(i=0; i<vtxSize; ++i)
  {
    vtxArray[i].x=Table_Index(vtxTable, i,0);
    vtxArray[i].y=Table_Index(vtxTable, i,1);
    vtxArray[i].z=Table_Index(vtxTable, i,2);
  
    //bounding box
    if (vtxArray[i].x<minx) minx=vtxArray[i].x;
    if (vtxArray[i].x>maxx) maxx=vtxArray[i].x;
    if (vtxArray[i].y<miny) miny=vtxArray[i].y;
    if (vtxArray[i].y>maxy) maxy=vtxArray[i].y;
    if (vtxArray[i].z<minz) minz=vtxArray[i].z;
    if (vtxArray[i].z>maxz) maxz=vtxArray[i].z;
  }

  //resizing and repositioning params
  double centerx=0, centery=0, centerz=0;
  if (!notcenter) {
    centerx=(minx+maxx)*0.5;
    centery=(miny+maxy)*0.5;
    centerz=(minz+maxz)*0.5;
  }
  
  double rangex=-minx+maxx,
         rangey=-miny+maxy,
         rangez=-minz+maxz;

  double ratiox=1,ratioy=1,ratioz=1;

  if (xwidth && rangex)
  {
    ratiox=xwidth/rangex;
    ratioy=ratiox;
    ratioz=ratiox;
  }

  if (yheight && rangey)
  {
    ratioy=yheight/rangey;
    if(!xwidth)  ratiox=ratioy;
    ratioz=ratioy;
  }

  if (zdepth && rangez)
  {
    ratioz=zdepth/rangez;
    if(!xwidth)  ratiox=ratioz;
    if(!yheight) ratioy=ratioz;
  }
  
  rangex *= ratiox;
  rangey *= ratioy;
  rangez *= ratioz;
  
  //center and resize the object 
  for (i=0; i<vtxSize; ++i)
  {
    vtxArray[i].x=(vtxArray[i].x-centerx)*ratiox+(!notcenter ? 0 : centerx);
    vtxArray[i].y=(vtxArray[i].y-centery)*ratioy+(!notcenter ? 0 : centery);
    vtxArray[i].z=(vtxArray[i].z-centerz)*ratioz+(!notcenter ? 0 : centerz);        
  }


  //table_read create a table on one line if the number of columns is not constant, so there are 2 cases :
  if (faceTable.rows==1)
  {
    //copy the table in a 1-row array
    faceArray=malloc(faceSize*sizeof(unsigned long));
    if (!faceArray) return(0);
    for (i=0; i<faceSize; ++i)
    {
      faceArray[i]=Table_Index(faceTable, 0, i);
    }
  }
  else
  {
    //read each row of the table and concatenate in a 1-row array
    faceArray=malloc(polySize*(faceSize)*sizeof(unsigned long));
    if (!faceArray) return(0);
    for(i=0; i<polySize; ++i)
    {
      for(j=0; j<faceSize; ++j)
        faceArray[i*(faceSize)+j]=Table_Index(faceTable, i, j);
    }
    faceSize*=polySize;  
  }

  //precomputes normals
  long indNormal=0;//index in polyArray
  i=0;    //index in faceArray
  while (i<faceSize)
  {  
    int    nbVertex=faceArray[i];//nb of vertices of this polygon
    double vertices[3*nbVertex];
    int j;
  
    for (j=0; j<nbVertex; ++j)
    {
      unsigned long indVertPj=faceArray[i+j+1];
      vertices[3*j]  =vtxArray[indVertPj].x;
      vertices[3*j+1]=vtxArray[indVertPj].y;
      vertices[3*j+2]=vtxArray[indVertPj].z;
    }
    
    polygon p;    
    p.p   =vertices;
    p.npol=nbVertex;
    off_normal(&(p.normal),p);
    
    normalArray[indNormal]=p.normal;
  
    i += nbVertex+1;
    indNormal++;  
  
  }

  if (ratiox!=ratioy || ratiox!=ratioz || ratioy!=ratioz)
    printf("Warning: Aspect ratio of the sample was modified.\n"
           "         If you want to keep the originial proportions, specifiy only one of the dimensions.\n");
  printf("  Bounding box dimensions:\n");
  printf("    Length=%f (%.3f%%)\n", rangex*ratiox, ratiox*100);
  printf("    Width= %f (%.3f%%)\n", rangey*ratioy, ratioy*100);
  printf("    Depth= %f (%.3f%%)\n", rangez*ratioz, ratioz*100);

  data->vtxArray   = vtxArray;
  data->normalArray= normalArray;
  data->faceArray  = faceArray;
  data->vtxSize    = vtxSize;
  data->polySize   = polySize;
  data->faceSize   = faceSize;
  return(polySize);
} /* off_init */

/*******************************************************************************
* int off_intersect(double* t0, double* t3, 
     Coords *n0, Coords *n3,
     double x, double y, double z, 
     double vx, double vy, double vz, 
     off_struct data )
* ACTION: computes intersection of neutron trajectory with an object. 
* INPUT:  x,y,z and vx,vy,vz are the position and velocity of the neutron
*         data points to the OFF data structure
* RETURN: the number of polyhedra which trajectory intersects
*         t0 and t3 are the smallest incoming and outgoing intersection times
*         n0 and n3 are the corresponding normal vectors to the surface
*******************************************************************************/
int off_intersect(double* t0, double* t3, 
     Coords *n0, Coords *n3,
     double x,  double y,  double z, 
     double vx, double vy, double vz, 
     off_struct data )
{
    intersection t[CHAR_BUF_LENGTH];
    Coords A={x, y, z};
    Coords B={x+vx, y+vy, z+vz};
    int t_size=off_clip_3D_mod(t, A, B,
      data.vtxArray, data.vtxSize, data.faceArray, data.faceSize, data.normalArray );
    qsort(t, t_size, sizeof(intersection),  off_compare);
    off_cleanDouble(t, &t_size);
    off_cleanInOut(t,  &t_size);

    if(t_size>0)
    {
      int i=0;
      if (t0) *t0 = t[0].time;
      if (n0) *n0 = t[0].normal;;
      if(t_size>1) {
        for (i=1; i < t_size; i++)
          if (t[i].time > 0 && t[i].time > t[0].time) break;
        if (t3) *t3 = t[i].time;
        if (n3) *n3 = t[i].normal;
      }
      return t_size;
    }
    return 0;
} /* off_intersect */

/*******************************************************************************
* void off_display(off_struct data)
* ACTION: display up to N_VERTEX_DISPLAYED polygons from the object
*******************************************************************************/
void off_display(off_struct data)
{
  unsigned int i;
  double ratio=(double)(N_VERTEX_DISPLAYED)/(double)data.faceSize;
  for (i=0; i<data.faceSize-1; i++) {
    int j;
    int nbVertex = data.faceArray[i];
    double x0,y0,z0;
    x0 = data.vtxArray[data.faceArray[i+1]].x;
    y0 = data.vtxArray[data.faceArray[i+1]].y;
    z0 = data.vtxArray[data.faceArray[i+1]].z;
    if (ratio > 1 || rand01() < ratio) {
      double x1=x0,y1=y0,z1=z0;
      for (j=2; j<=nbVertex; j++) {
        double x2,y2,z2;
        x2 = data.vtxArray[data.faceArray[i+j]].x;
        y2 = data.vtxArray[data.faceArray[i+j]].y;
        z2 = data.vtxArray[data.faceArray[i+j]].z;
        mcdis_line(x1,y1,z1,x2,y2,z2);
        x1 = x2; y1 = y2; z1 = z2;
      }
      mcdis_line(x1,y1,z1,x0,y0,z0);
    }
    i += nbVertex;
  }
} /* off_display */

/* end of interoff-lib.c */

