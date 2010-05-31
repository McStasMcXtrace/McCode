
//gives the normal vector of p
void normal(Coords* n, polygon p)
{
        //using Newell method  
	long i;
	n->x=0;n->y=0;n->z=0;
	for(i=0;i<p.npol;++i)
	{
		double x1=p.p[i].x,
		       y1=p.p[i].y, 
		       z1=p.p[i].z;
		double x2=p.p[(i+1)%p.npol].x,
		       y2=p.p[(i+1)%p.npol].y, 
		       z2=p.p[(i+1)%p.npol].z;
		// n is the cross product of v1*v2
		n->x += (y1 - y2) * (z1 + z2);
        	n->y += (z1 - z2) * (x1 + x2);
        	n->z += (x1 - x2) * (y1 + y2);
	}
}

//scalar product
double scalarp(Coords a, Coords b)
{
        return a.x*b.x + a.y*b.y + a.z*b.z;
}

//based on http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
//return 0 if the Coords is out
//	 1 if it is in
//	 -1 if on the boundary
int pnpoly(polygon p, Coords v)
{
      int i, j, c = 0;
      double minx=DBL_MAX,maxx=-DBL_MAX,miny=DBL_MAX,maxy=-DBL_MAX,minz=DBL_MAX,maxz=-DBL_MAX;
      double rangex,rangey,rangez;
      double* polx,*poly,*polz;
      double x=v.x,y=v.y;
      polx=malloc(p.npol*sizeof(double));
      poly=malloc(p.npol*sizeof(double));
      polz=malloc(p.npol*sizeof(double));
      //take the most relevant 2D projection (prevent from instability)
      for(i=0;i<p.npol;++i)
      {
	  polx[i]=p.p[i].x;
	  poly[i]=p.p[i].y;
	  polz[i]=p.p[i].z;
	  if(polx[i]<minx)minx=polx[i];
	  if(polx[i]>maxx)maxx=polx[i];
	  if(poly[i]<miny)miny=poly[i];
	  if(poly[i]>maxy)maxy=poly[i];
	  if(polz[i]<minz)minz=polz[i];
	  if(polz[i]>maxz)maxz=polz[i];	
      }
      rangex=maxx-minx;
      rangey=maxy-miny;
      rangez=maxz-minz;
      if(rangex<rangez)
      {
	  if(rangex<rangey) 
          {
		polx=polz;
                x=v.z;
		//printf("yz\n");
          }
	  else
	  {
		poly=polz;
                y=v.z;
		//printf("xz\n");
          }
      }
      else if(rangey<rangez)
      {
	  poly=polz;
          y=v.z;	  
		//printf("xz\n");
      }
	//else printf("xy\n");
	//printf("x=%f,y=%f,z=%f\n",rangex,rangey,rangez);
      //trace rays and test number of intersection
      for (i = 0, j = p.npol-1; i < p.npol; j = i++) {
        if (((((poly[i])<=y) && (y<(poly[j]))) ||
             (((poly[j])<=y) && (y<(poly[i])))) &&
            (x < ( (polx[j] - polx[i]) * (y - poly[i]) / (poly[j] - poly[i]) + polx[i])))	
          c = !c;

	if (((fabs(poly[i]-y)<=EPSILON) || ((fabs(poly[j]-y)<=EPSILON))) &&
            fabs(x -((polx[j] - polx[i]) * (y - poly[i]) / (poly[j] - poly[i]) + polx[i])) < EPSILON)
	{
		//the point lies on the edge
		c=-1;
		break;
	}
		
	//printf("%d\n",c);
      }

      return c;
}

//gives the intersection Coords between ray [a,b) and polygon p and its prametric value on (a b)
//http://geometryalgorithms.com/Archive/algorithm_0105/algorithm_0105.htm
int intersectPoly(intersection *inter, Coords a, Coords b, polygon p)
{
        //direction vector of [a,b]
        Coords dir;
        dir.x = (b.x-a.x);
        dir.y = (b.y-a.y);
        dir.z = (b.z-a.z);
        
        //the normal vector to the polygon
        Coords normale;
        normal(&normale, p);       
        
        //direction vector from a to a Coords of the polygon
        Coords w0;
        w0.x = (a.x-p.p[0].x);
        w0.y = (a.y-p.p[0].y);
        w0.z = (a.z-p.p[0].z);

        //scalar product
        double nw0 = -scalarp(normale,w0);
        double ndir = scalarp(normale,dir);
        if (fabs(ndir) < EPSILON)          // ray is parallel to polygon plane
        {     
                if (nw0 == 0)                // ray lies in polygon plane (infinite number of solution)
                    return 0;
                else return 0;             // ray disjoint from plane (no solution)
        }

        // get intersect point of ray with polygon plane
        inter->time = nw0 / ndir;            //parametric value the point on line (a,b)
        if (inter->time < 0.0)               // ray goes away from polygon
                return 0;          // => no intersect

	//printf("----------t=%f",*t);
        inter->v.x = a.x + inter->time * dir.x;// intersect point of ray and plane
        inter->v.y = a.y + inter->time * dir.y;//
	inter->v.z = a.z + inter->time * dir.z;//
        
//	if(pnpoly(p,*inter)==1)
//		printf("%s\n",(ndir<0)?"in":"out");

	int res=pnpoly(p,inter->v);
	
	inter->edge=(res==-1);
	if(ndir<0)
		inter->in_out=1;		//the negative dot product means we enter the surface
	else
		inter->in_out=-1;
	
	return res;	//true if the intersection point lies inside the poly
}


/*reads the indexes at the beginning of the off file as this :
line 1  OFF
line 2  nbVertex nbFaces nbEdges
*/
void getBlocksIndex(char* filename, unsigned long* vtxIndex, unsigned long* vtxSize, unsigned long* faceIndex )
{
       FILE* f = fopen(filename,"r");
        char line[buf];
 	*vtxIndex=0;
        *vtxSize=0;
        *faceIndex=0;
        fgets(line,buf , f);// line 1 = "OFF"
        *vtxIndex+= strlen(line);
          
        fgets(line,buf,f); //line 2 = nblines of Coords,faces and edges arrays
        *vtxIndex+=strlen(line);
        sscanf(line,"%d",vtxSize);

        *faceIndex=*vtxIndex;
        int i;
        for(i=0;i<*vtxSize;++i)
        {                              
                fgets(line,buf,f);
                *faceIndex+=strlen(line);                
        }
       
        fclose(f);
}

double F(Coords v, double A, double B, double C, double D)
{
	return A*v.x + B*v.y + C*v.z + D;
}

//gives the equations of 2 perpandicular planes of [ab]
void init_planes(Coords a, Coords b, double* A1, double* C1, double* D1, double *A2, double* B2, double* C2, double* D2)
{
	//direction vector of [a b]	
	Coords dir={b.x-a.x, b.y-a.y, b.z-a.z};
	
	//the plane parallel to the 'y' is computed with the normal vector of the projection of [ab] on plane 'xz'	
	*A1=dir.z;
	*C1=-dir.x;
	if(*A1!=0 || *C1!=0)
		*D1=-(a.x)**A1-(a.z)**C1;
	else
	{
		//the plane do not support the vector, take the one parallel to 'z''
		*A1=1;
		//B1=dir.x=0
		*D1=-(a.x);
	}
	//the plane parallel to the 'x' is computed with the normal vector of the projection of [ab] on plane 'yz'	
	*B2=dir.z;
	*C2=-dir.y;
	*A2=0;
	if(*B2==0 && *C2==0)
	{
		//the plane do not support the vector, take the one parallel to 'z'
		*B2=1;
		//B1=dir.x=0
		*D2=-(a.y);
	}
	else if(dir.z==0)
	{
		//the planes are the same, take the one parallel to 'z'
		*A2=dir.y;
		*B2=-dir.x;
		*D2=-(a.x)**A2-(a.y)**B2; 
	}
	else
		*D2=-(a.y)**B2-(a.z)**C2;

	//printf("F1=%f\n",F(b,*A1,0,*C1,*D1));
	//printf("F2=%f\n",F(b,*A2,*B2,*C2,*D2));
}


int sign(double a)
{
	return (a<0)?-1:(a!=0);
}	

int clip_3D_mod(intersection* t, Coords a, Coords b, t_Table vtxTable, t_Table faceTable )
{
	double A1, C1, D1, A2, B2, C2, D2;			//perpandicular plane equations to [a,b]
	init_planes(a, b, &A1, &C1, &D1, &A2, &B2, &C2, &D2);	//	
	
	int t_size=0;
	unsigned long vtxSize=vtxTable.rows, faceSize=faceTable.columns;	//Size of the corresponding tables
	int sg[vtxSize];	//array telling if Coords is left or right of the plane
	
	int i;
	for(i=0; i < vtxSize; ++i)
	{
		Coords x={	Table_Index(vtxTable, i,0),
				Table_Index(vtxTable, i,1),
				Table_Index(vtxTable, i,2)};
		sg[i]=sign(F(x,A1,0,C1,D1));
	}

	//exploring the polygons :
	i=0;
	while(i<faceSize)
	{	
		polygon pol;
		pol.npol=Table_Index(faceTable, 0,i);			//nb Coords of polygon
		pol.p=malloc(pol.npol*sizeof(Coords));
		unsigned long indVertP1=Table_Index(faceTable, 0,++i);	//polygon's first Coords index in vtxTable
		int j=1;
		while(j<pol.npol)
		{
			long indVertPi=Table_Index(faceTable, 0, i+j);	//polygon's j-th Coords index in vtxTable
			if(sg[indVertP1]!=sg[indVertPi])		//if the plane intersect the polygon
				break;	

			++j;
		}
		
		if(j<pol.npol)						//ok, let's test with the second plane
		{	
			Coords x1={	Table_Index(vtxTable, indVertP1 ,0),	//Coords coordinates
					Table_Index(vtxTable, indVertP1 ,1),	//
					Table_Index(vtxTable, indVertP1 ,2)};	//			

			int sg1=sign(F(x1,A2,B2,C2,D2));	//tells if Coords is left or right of the plane	
			//printf("prout %d\n",sign(F(x1,0,B2,C2,D2)));
			j=1;		
			while(j<pol.npol)
			{
				long indVertPi=Table_Index(faceTable, 0, i+j);	//polyg's j-th Coords index in vtxTable
				Coords xj={	Table_Index(vtxTable, indVertPi ,0),	//Coords coordinates
						Table_Index(vtxTable, indVertPi ,1),	//
						Table_Index(vtxTable, indVertPi ,2)};	//				
				
				if(sg1!=sign(F(xj,A2,B2,C2,D2)))		//if the plane intersect the polygon
					break;
				++j;
			}
			if(j<pol.npol)
			{
				//both planes intersect the polygon, let's find the intersection point
				//our polygon :
				int k;//printf("\n");
				for(k=0;k<pol.npol;++k)
				{	
					unsigned long indVertPk=Table_Index(faceTable, 0, i+k);	//k-th Coords index in vtxTable
					pol.p[k].x = Table_Index(vtxTable, indVertPk ,0),	//Coords coordinates
					pol.p[k].y = Table_Index(vtxTable, indVertPk ,1),	//
					pol.p[k].z = Table_Index(vtxTable, indVertPk ,2);	//
					//printf("%f %f %f\n",pol.p[k].x,pol.p[k].y,pol.p[k].z);
				}
				
				intersection x;
				if(intersectPoly(&x, a, b, pol))
				{						
					t[t_size]=x;
					++t_size;
				}
			}	
		}
		i+=pol.npol;
	}
	return t_size;
}


int compare (void const *a, void const *b)
{
   intersection const *pa = a;
   intersection const *pb = b;

   return sign(pa->time - pb->time);
}

//given an array of intesction throw those which appear several times
//returns 1 if there is a possibility of error
int cleanDouble(intersection* t, int* t_size)
{
	int i=1;
	intersection prev;
	prev=t[0];
	while(i<*t_size)	
	{
		int j=i;
		//for each intersection with the same time
		while(j<*t_size && fabs(prev.time-t[j].time)<EPSILON)
		{
			//if the intersection is the exact same erase it
			if(prev.in_out==t[j].in_out)
			{
				int k;			
				for(k=j+1;k<*t_size;++k)
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
}


//given an array of intesction throw those which enter and exit in the same time
//Meaning the ray passes very close to the volume
//returns 1 if there is a possibility of error
int cleanInOut(intersection* t, int* t_size)
{
	int i=1;
	intersection prev;
	prev=t[0];
	while(i<*t_size)	
	{
		//if two intersection have the same time but one enters and the other exits erase both
		//(such intersections must be adjacent in the array : run cleanDouble before)
		if(fabs(prev.time-t[i].time)<EPSILON && prev.in_out!=t[i].in_out)
		{
			int j;		
			for(j=i+1;j<*t_size;++j)
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
	return 1;
}

/*
int main()
{
        t_Table vtxTable, faceTable;
        unsigned long vtxIndex, vtxSize, faceIndex;

        // get the indexes
        getBlocksIndex(file,&vtxIndex,&vtxSize,&faceIndex);
       
        //read Coords table = [x y z | x y z | ...]
        Table_Read_Offset(&vtxTable, file, 0, &vtxIndex, vtxSize);

        //Table_Info(vtxTable);
        
        // Data is (double)table.data of size table.rows x table.columns 

        //double a = Table_Index(vtxTable, 0,0); /* first element of table 
        //double b = Table_Index(vtxTable, vtxSize, 0); /* last element of table 
        //printf("%f %f\n",a,b);

        //read face table = [nbvertex v1 v2 vn nbvertex v1 v2 vn ...]
        Table_Read_Offset(&faceTable, file, 0, &faceIndex, 0);

        //Table_Info(faceTable);
        
        // Data is (double)table.data of size table.rows x table.columns 

        //int c = Table_Index(faceTable, 0,0); // first element of table 
        

	//Coords A={34,-90,-60};
	//Coords B={23,74,53};
	Coords A={100 ,-26.498593, 12.98};
	Coords B={3.541977, -1.032849, 0.289808};
	intersection t[200];
        

	int i, t_size;    

	t_size=clip_3D_mod(t, A, B,vtxTable, faceTable );
	qsort(t,t_size,sizeof(intersection),compare);
	cleanDouble(t,&t_size);
	cleanInOut(t,&t_size);
	if(GEOMVIEW)
	{
		printf("OFF\n%d %d 0\n-200 0 0\n200 0 0\n0 -200 0\n0 200 0\n0 0 -200\n0 0 200\n",t_size*6+8,t_size*3+4);
		printf("%f %f %f\n",A.x,A.y,A.z);
		printf("%f %f %f\n",B.x,B.y,B.z);

		for(i=0;i<t_size;++i)
		{
			printf("%f %f %f\n",t[i].v.x-5,t[i].v.y,t[i].v.z);
			printf("%f %f %f\n",t[i].v.x+5,t[i].v.y,t[i].v.z);
			printf("%f %f %f\n",t[i].v.x,t[i].v.y+5,t[i].v.z);
			printf("%f %f %f\n",t[i].v.x,t[i].v.y-5,t[i].v.z);
			printf("%f %f %f\n",t[i].v.x,t[i].v.y,t[i].v.z+5);
			printf("%f %f %f\n",t[i].v.x,t[i].v.y,t[i].v.z-5);
		}			
		printf("2 0 1\n2 2 3\n2 4 5\n2 6 7\n");
		for(i=0;i<t_size;++i)
		{
			//printf("t=%f\n",t[i].time);
			//printf("%s\n",(t[i].in_out==1)?"in":"out");
			printf("2 %d %d\n",8+i*6,8+i*6+1);
			printf("2 %d %d\n",8+i*6+2,8+i*6+3);
			printf("2 %d %d\n",8+i*6+4,8+i*6+5);		
		}
	}

	
}
*/

