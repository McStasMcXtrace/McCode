function mainPlot3d(typeOfPlot,argList)


// typeOfPlot can take the following values :
//
// plot3
// (tri)mesh
// (tri)surf
// (tri)surfl
// (tri)pcolor
// triplot
// fill

hidden=%T;
numberOfVertices=4;     // default type of polygons
lightVect=[1;1;1];      // default position of light source (at infinity)
azimuth=45;             // default azimuth
elevation=54.7;         // default elevation
facecolor=[];           // default facecolor for 'mesh'
BackFaceColor="none";
BackFaceCulling="no";
edgecolor=[];           // default edgecolor for 'mesh'
markersize=1;
shadingType='faceted';
dejavu=0;
surfaceIsParam=%F;
colorBar='off'
Xlabel=' ';
Ylabel=' ';
Zlabel=' ';
Title=[];
matOfLegends=[];
ticksx=[];
ticksy=[];

gridFlag=%f;
gridColor=[];

minX=%inf;
maxX=-%inf;
minY=%inf;
maxY=-%inf;
minZ=%inf;
maxZ=-%inf;

listOfTriples=list();
currentColor=1;
argNumber=1;
marker=[];

axisRatio='auto';
axisTightX=%F;
axisTightY=%F;
axisStyle='normal';
axisVect=[];

foreground=[];
background=[];

if winsid()==[]
  fig();
end

savedThickness=xget('thickness');

win=xget('window'); // get the number of current graphic window
xset('auto clear','off')  

while length(argList)

	if type(argList(1))==15 // If the first element is itself a list 

		tempList=argList(1); // expand this list in the main argument list
		argList(1)=null();
		for i=length(tempList):-1:1
			argList(0)=tempList(i);
		end
	end

	if type(argList(1))==1 // If the first argument is a matrix

		// then the second argument must be also a matrix.
		// The 3rd argument can be a matrix or a macro (function)

		if length(argList)==1 & typeOfPlot~='plot3'
			s=size(argList(1));
			argList(0)=1:s(1); // insert an abscissa vector of size n,
			argList(0)=1:s(2); // insert an ordina vector of size m,
			argNumber=argNumber-2;
		end

		if length(argList)>=2
			if type(argList(2)) ~= 1
				s=size(argList(1));
				argList(0)=1:s(1); // insert an abscissa vector of size n,
				argList(0)=1:s(2); // insert an ordina vector of size m,
				argNumber=argNumber-2;
			end
		end

		if length(argList)>=3

			if type(argList(2))==1 & (type(argList(3))==1 | type(argList(3))==13)

			// We verify (X,Y,Z,C) consistency, and eventually generate the 'real' Z
			// if Z was a function in the input argument list (mesh or surf case)

				if typeOfPlot=='trimesh' | ...
					typeOfPlot=='trisurf' | ...
					typeOfPlot=='trisurfl' | ...
					typeOfPlot=='tripcolor'


					if length(argList)>=5
						if (type(argList(5))==1 | type(argList(5))==13) // a matrix or a function for the color
							 [X,Y,Z,triang]=checkXYZQuadruple(typeOfPlot,argList(1),argList(2),...
		                    							   argList(3),argList(4),argList(5));
							argList(1)=null();argList(1)=null();argList(1)=null();argList(1)=null();
							argList(1)=null();
							argNumber=argNumber+5;
						else
							[X,Y,Z,triang]=checkXYZQuadruple(typeOfPlot,argList(1),argList(2),...
		                    							   argList(3),argList(4));
							argList(1)=null();argList(1)=null();argList(1)=null();argList(1)=null();
							argNumber=argNumber+4;
						end

					else
						[X,Y,Z,triang]=checkXYZQuadruple(typeOfPlot,argList(1),argList(2),...
		                    						   argList(3),argList(4));
						argList(1)=null();argList(1)=null();argList(1)=null();argList(1)=null();
						argNumber=argNumber+4;
					end		  

				elseif ((typeOfPlot=='surf') | (typeOfPlot=='fill3') | (typeOfPlot=='pcolor')) & length(argList)>=4 

					if (type(argList(4))==1 | type(argList(4))==13) // a matrix or a function for the color
						[X,Y,Z,surfaceIsParam]=checkXYZQuadruple(typeOfPlot,argList(1),argList(2),...
		                    						   argList(3),argList(4));
						argList(1)=null();argList(1)=null();argList(1)=null();argList(1)=null();
						argNumber=argNumber+4;
					else
						[X,Y,Z,surfaceIsParam]=checkXYZQuadruple(typeOfPlot,argList(1),argList(2),argList(3));
						argList(1)=null();argList(1)=null();argList(1)=null();
						argNumber=argNumber+3;
					end
				
				elseif typeOfPlot=='triplot'
					
					[X,Y,Z,triang]=checkXYZQuadruple(typeOfPlot,argList(1),argList(2),argList(3));
					argList(1)=null();argList(1)=null();argList(1)=null();
					argNumber=argNumber+3;
					
				else // e.g. "surf,surfl, fill, ..."

					[X,Y,Z,surfaceIsParam]=checkXYZQuadruple(typeOfPlot,argList(1),argList(2),argList(3));
					argList(1)=null();argList(1)=null();argList(1)=null();
					argNumber=argNumber+3;

				end
				

				if type(Z)==1 // includes Z==[]
					minX=min(minX,min(X));         maxX=max(maxX,max(X));
					minY=min(minY,min(Y));         maxY=max(maxY,max(Y));
					if Z==[]
						minZ=[];maxZ=[];
					else	    
						minZ=min(minZ,min(real(Z)));   maxZ=max(maxZ,max(real(Z)));
					end
					axisVect=[minX maxX minY maxY minZ maxZ];
				end

				_color=0; lineStyle=1; marker=[];

				if length(argList) & typeOfPlot=='plot3' // If there is a next argument
					if type(argList(1))==10 // If this argument is a string
	    			 						// and this is a plot3() call
						[_color,marker,markersize,lineStyle,fail]=getColorNumber(argList(1)); // get the color number
						if ~fail // the string seems to be a maker/color combination
							argList(1)=null(); // Delete the top argument in the argument list
							argNumber=argNumber+1;
						else
							_color=0; lineStyle=1; marker=[];
						end  
					end
				end

				if typeOfPlot=='plot3'
					[currentColor,listOut]=addPlot3(X,Y,Z,_color,marker,markersize,lineStyle,win);
					listOfTriples(0)=listOut;
				elseif dejavu==0
					dejavu=1;
				else
					error(sprintf('%s : too many input arguments',typeOfPlot));
				end

			else
				error(sprintf('%s : arguments %d,%d and %d must be matrices',...
				typeOfPlot,argNumber,argNumber,argNumber+1));
			end

		else
		  error(sprintf('%s : not enough input arguments',typeOfPlot))      
		end
      
   elseif (type(argList(1))==10) // If this argument is a string

      select convstr(argList(1)) // Try to identify a property name

      case 'colorbar'
         colorBar=parseColorBar(typeOfPlot,argList);
	 argList(1)=null(); argList(1)=null();
      case 'light'
         lightVect=parseLight(typeOfPlot,argList);
	 argList(1)=null(); argList(1)=null();
      case 'backfacelighting'
         BackFaceColor=parseBackFaceLighting(typeOfPlot,argList);
	 argList(1)=null(); argList(1)=null();
      case 'backfacecolor'
         BackFaceColor=parseColor(typeOfPlot,'backfacecolor',argList);
	 argList(1)=null(); argList(1)=null();
      case 'backfaceculling'
         BackFaceCulling=parseBackFaceCulling(typeOfPlot,argList);
	 argList(1)=null(); argList(1)=null();
      case 'view'
         [azimuth,elevation]=parseView(typeOfPlot,argList,azimuth,elevation);
	 argList(1)=null(); argList(1)=null();
      case 'facecolor'
         facecolor = parseColor(typeOfPlot,'facecolor',argList);
         argList(1)=null(); argList(1)=null();
      case 'edgecolor'
         edgecolor = parseColor(typeOfPlot,'edgecolor',argList);
         argList(1)=null(); argList(1)=null();
      case 'shading'
         [facecolor,edgecolor]=parseShading(typeOfPlot,argList);
	 argList(1)=null(); argList(1)=null();
      case 'axis'
         [axisStyle,axisRatio,axisVect,axisTightX,axisTightY] = ...
	 parseAxis(typeOfPlot,argList,axisStyle,axisRatio,axisVect,axisTightX,axisTightY);
	 argList(1)=null(); argList(1)=null();
      case 'ticksx'
         [ticksx] = parseTicks(typeOfPlot,argList);
	       
	 argList(1)=null(); argList(1)=null();

      case 'ticksy'
         [ticksx] = parseTicks(typeOfPlot,argList);
	       
	 argList(1)=null(); argList(1)=null();
      case 'background'
         background = parseColor(typeOfPlot,'background',argList);
         argList(1)=null(); argList(1)=null();
      case 'foreground'
         foreground = parseColor(typeOfPlot,'foreground',argList);
         argList(1)=null(); argList(1)=null();
      case 'xlabel'
         Xlabel = parseLabel(typeOfPlot,'xlabel',argList);
	 argList(1)=null(); argList(1)=null();
      case 'ylabel'
         Ylabel = parseLabel(typeOfPlot,'ylabel',argList);
	 argList(1)=null(); argList(1)=null();
      case 'title'
         Title = parseLabel(typeOfPlot,'title',argList);
	 argList(1)=null(); argList(1)=null();
      case 'zlabel'
         Zlabel = parseLabel(typeOfPlot,'zlabel',argList);
	 argList(1)=null(); argList(1)=null();
      case 'hidden'
         hidden = parseHideMode(typeOfPlot,argList);
	 argList(1)=null(); argList(1)=null();
      case 'grid'
         [gridFlag,gridColor]=parseGrid(typeOfPlot,argList);
	 argList(1)=null(); argList(1)=null();
     case 'legend'
       argList(1)=null();
 	     [matOfLegends,nbProc,typeOfLegend]=parseLegend(typeOfPlot,argList);         
       if nbProc==0
         error('bar : missing string(s) for legend');
	     end
       for k=1:nbProc; argList(1)=null(); end;

      else
         error(sprintf('%s : %s is an unknown property name',typeOfPlot,argList(1)));
      end // select argList(1)
      
   else
      str=sprintf('%s : argument %d has not the expected type',typeOfPlot,argNumber);
      error(str);

   end // if type(argList(1))
end // while length(argList)

// Common 2D/3D stuff

[foreground,background]=processSFB(foreground,background,win,typeOfPlot)

if edgecolor==[]
   edgecolor=foreground;
end

if typeOfPlot=='mesh' | typeOfPlot=='trimesh' | typeOfPlot=='triplot'
	if facecolor==[]
	   facecolor=background;
	end
else
	if facecolor==[]
	   facecolor="flat";
	end
end

labels=Xlabel+'@'+Ylabel+'@'+Zlabel;

state=loadGraphicState(win)
state('typeOfPlot')=typeOfPlot;

// Now we process the list of plots to do

if typeOfPlot=='plot3'
   for k=1:length(listOfTriples)
      [X,Y,Z,col,markerId,markersize,lineStyle]=listOfTriples(k)(2:8)
      xset('thickness',savedThickness);

      if k==1 & state('nextPlot')=='erase'
         [modeStart]=process3DPrelim(win,axisVect,axisRatio,axisStyle,[],[]);
	 xset('foreground',addcolor(foreground));
         xset('background',addcolor(state('frameColor')));
         xset('pattern',xget('foreground'));
         param3d1(0,0,0,azimuth,elevation,labels,[modeStart],axis());
      end
      
      xset('thickness',markersize);
      xset('line style',lineStyle);

      if markerId ~=[]
         xset('dashes',col);
         xset('mark',markerId,markersize);
         param3d1(X,Y,list(Z,-markerId),azimuth,elevation,' ',[0 0]);
         if lineStyle>0
            xset('thickness',markersize);
	    xset('line style',lineStyle);
	    param3d1(X,Y,list(Z,col),azimuth,elevation,' ',[0 0]);
         end
         xset('dashes',0)
      else
         param3d1(X,Y,list(Z,col),azimuth,elevation,' ',[0 0]);
      end
      xset('thickness',savedThickness);
   end
else

   tab=state('RGBcolormaptable');
   nc=length(tab);

   if type(Z)==1 // if the surface/patch is defined by numerical data

		if typeOfPlot=="fill" | typeOfPlot=="bar"
        elseif typeOfPlot=='trisurfl' | ...
			typeOfPlot=='trisurf' | ...
			typeOfPlot=='tripcolor' | ...
			typeOfPlot=='trimesh' |...
			typeOfPlot=='triplot'

			ntri=size(triang,2);
			triang=triang($:-1:1,:);
			nnodes=length(X);

			if typeOfPlot=='tripcolor' & length(Z)~=length(X) // when color is given for triangles, not nodes
				Z=Z(:)';
				if facecolor=='interp' // Averaging of value for each node
					C=zeros(nnodes,1);
					nV=zeros(nnodes,1);
					t=zeros(3,1);
					M=[-1 -1 0;1 0 1;0 1 -1];
					for i=1:ntri // for each triangle, nodal values are weighted
						nodes=triang(:,i); // by the measure of the associated angular sector
						D=[X(nodes);Y(nodes)]*M;
						D=D*diag(1../sqrt(sum(D.^2,'r')));
						s=D'*D;
						t=acos(s(2:3,1));t(3)=%pi-[1 1]*t;
						nV(nodes)=nV(nodes)+t;
						C(nodes)=C(nodes)+Z(i)*t;
					end
					Z=C./(nV+%eps);
					clear C;
					Z=matrix(Z(triang),3,ntri);
				end
			elseif typeOfPlot~='triplot'
				Z=matrix(Z(triang),3,ntri);
			end

			X=matrix(X(triang),3,ntri); 
			Y=matrix(Y(triang),3,ntri);

			if typeOfPlot=='trisurfl' // Computation of normals and illumination

				N=-[(Y(2,:)-Y(1,:)).*(Z(3,:)-Z(1,:))-(Z(2,:)-Z(1,:)).*(Y(3,:)-Y(1,:));
				(Z(2,:)-Z(1,:)).*(X(3,:)-X(1,:))-(X(2,:)-X(1,:)).*(Z(3,:)-Z(1,:));
				(X(2,:)-X(1,:)).*(Y(3,:)-Y(1,:))-(Y(2,:)-Y(1,:)).*(X(3,:)-X(1,:))];

				N=N*sparse([1:ntri;1:ntri]',1../(%eps+sqrt(sum(N.^2,'r'))));

				if facecolor=='interp' // Averaging of illumination for each node

					L=computeLight(N,lightVect);
					clear N;
					C=zeros(nnodes,1);
					nV=zeros(nnodes,1);
					for i=1:ntri
						nodes=triang(:,i);
						C(nodes)=C(nodes)+L(i);
						nV(nodes)=nV(nodes)+1;
					end
					clear L;
					C=C./(nV+%eps);
					Z=Z+%i*matrix(C(triang),3,ntri);
					clear nV C
				else
					C=computeLight(N,lightVect);
					clear N;
				end

			end

		elseif ~surfaceIsParam    // non-parametric case

			nx=length(X);
			ny=length(Y);
			if typeOfPlot=='surfl'
				[zx,zy]=nonParametricDiffData(X,Y,Z');
				Z=Z+%i*matrix(computeLight(nonParametricNormals(zx,zy),lightVect),ny,nx)';
			end
			[X,Y,Z]=genfac3d(X,Y,Z);
		else

			[nv,nu]=size(X)          // parametric case
			if typeOfPlot=='surfl'
			   [xu,yu,zu,xv,yv,zv]=parametricDiffData(X,Y,Z);
			   Z=Z+%i*matrix(computeLight(parametricNormals(xu,yu,zu,...
						xv,yv,zv),lightVect),nv,nu);
            end
		 
	  	    // Now convert X,Y and Z to polygons	
 
		    if typeOfPlot=='fill3'
            else
                [X,Y,Z]=generate3dPolygons(X,Y,Z,numberOfVertices,surfaceIsParam);                
            end    
        end
      
	elseif type(Z)==13 // if the surface is defined by a function

		Zf=Z; clear Z;

		if typeOfPlot=='surfl'
			truc=lighten(Zf,lightVect);
			[X,Y,Z]=eval3dPolygons(X,Y,truc,numberOfVertices,surfaceIsParam);
		else
			[X,Y,Z]=eval3dPolygons(X,Y,Zf,numberOfVertices,surfaceIsParam);
		end

		if typeOfPlot=='pcolor'
			ax(1)=min(X); ax(2)=min(Y);
			ax(3)=max(X); ax(4)=max(Y);
			axisVect=[ax(1) ax(3) ax(2) ax(4)];
		else         
			ax(1)=min(X); ax(2)=min(Y); ax(3)=min(real(Z));
			ax(4)=max(X); ax(5)=max(Y); ax(6)=max(real(Z));
			axisVect=[ax(1) ax(4) ax(2) ax(5) ax(3) ax(6)];
		end

		state('axis')=ax;

	end

	if edgecolor~='none'
	    xset('dashes',addcolor(edgecolor));
	end
    
	if typeOfPlot=='mesh' | typeOfPlot=='trimesh'

		if hidden
			c=addcolor(facecolor);   
		else
			c=0;
		end

        	if BackFaceCulling=="yes"
	            xset('hidden3d',0);
        	else
			if BackFaceColor=="none";
				xset('hidden3d',-1);
       		        else 
            			xset('hidden3d',addcolor(BackFaceColor));
			end            
        	end


		[modeStart]=process3DPrelim(win,axisVect,axisRatio,axisStyle,[],[]);
		doThePlot3d(X,Y,Z,azimuth,elevation,labels,[c modeStart],axis());

	elseif 	typeOfPlot=='surf' | ...
			typeOfPlot=='pcolor' | ...
			typeOfPlot=='fill' | ...
			typeOfPlot=='bar' | ...
			typeOfPlot=='tripcolor' | ...
	  		typeOfPlot=='surfl' | ...
	  		typeOfPlot=='trisurf' | ...
	  		typeOfPlot=='trisurfl' | ...
			typeOfPlot=="triplot" | ...
            typeOfPlot=="fill3"



		    if typeOfPlot ~= "triplot"

			    if typeOfPlot=='trisurfl'

				    if facecolor=='interp'
					    C=imag(Z);
					    Z=real(Z);
				    end

			    else
				    if isreal(Z)
					    C=Z;
				    else
					    C=imag(Z); Z=real(Z);   
				    end

				    if facecolor=='flat'
					    C=sum(C,'r')/size(C,1);
    //					C=C(1,:);
				    end

			    end

			    minC=min(C);
			    maxC=max(C);

            if typeOfPlot=="bar"
                nSubBars=maxC;
	            LOP=list();
                if facecolor=='flat'
                    barColors=1:maxC;
			        if maxC==1
			           barColors=1;
			        else
			           barColors=round((barColors-1)/(maxC-1)*(nc-1))+1;
			        end
			        for k=maxC:-1:1
                        LOP(0)=list([],[],tab(barColors(k)),[],[],[])
 			        end
                elseif facecolor=="none"
                else // facecolor is a colorspec triple
			        for k=maxC:-1:1
                        LOP(0)=list([],[],addcolor(facecolor),[],[],[])
 			        end                                             
                end
		        state('listOfPlots')=LOP;
                saveGraphicState(state,win)      
            end

            if minC==maxC
	            if minC==0
		            minC=-1; maxC=1;
	            elseif minC<0
		            maxC=0;
		            minC=2*minC;
	            else
		            minC=0;
		            maxC=maxC*2;
	            end
            end

			if state('caxisMode')=='auto' // if the color axis is automatic

				C=round((C-minC)/(maxC-minC+%eps)*(nc-1))+1;
				state('caxis')=[minC maxC];
				saveGraphicState(state,win); // save the state of the current window

			else // the color axis has been held

				minCmaxC=state('caxis');
				minC=minCmaxC(1);
				maxC=minCmaxC(2);
				C=round((C-minC)/(maxC-minC+%eps)*(nc-1))+1;
				C(C<1)=1; C(C>nc)=nc; // treshold the colors

			end

			if edgecolor=='none'
				flag=-1;
			else
				flag=1;
			end

			if facecolor=='interp'
				C=matrix(tab(C),size(Z));
			elseif facecolor=='flat' | facecolor=="none"
				C=tab(C);
            else // facecolor is a colorspec
    			C=addcolor(facecolor);
            	C=C(ones(1,size(X,2)));
			end

		else
			C=zeros(1,size(X,2));
			if facecolor ~= 'none'
				C=C+addcolor(facecolor);
			end
		end

		if typeOfPlot=='pcolor'| typeOfPlot=='fill'| typeOfPlot=='bar'| typeOfPlot=='tripcolor' | typeOfPlot=='triplot'

			[modeStart,modeAdd,modeScale,nTicksX,nTicksY]=process2DPrelim(win,[],'linear','linear',...
			axisVect(1:4),axisTightX,axisTightY,axisRatio,axisStyle,colorBar,tab,[],[],ticksx,ticksy)

			state=loadGraphicState(win);

			// if first (X,Y) pair of the list and 'erase' property on, 
			// plot the axis and all the stuff
			process2DAxis(state,nTicksX,nTicksY,foreground,background,modeStart,modeScale,gridFlag,gridColor)
			
			xclip('clipgrf')
         
            if facecolor=='interp' & edgecolor=='none'
 //           	if edgecolor~='none'
			       C=-C;
 //               end
            elseif edgecolor=='none'
				     C=-C;		 
            end            
            
            if typeOfPlot=='bar' 
              if facecolor=='none'
                if edgecolor==foreground
              	   xpolys([X;X(1,:)],[Y;Y(1,:)],C)
                else
                   C=addcolor(edgecolor);
                   C=C(ones(1,size(X,2)));
                   xpolys([X;X(1,:)],[Y;Y(1,:)],C)
                end
              else
				xfpolys(X,Y,C)              
              end                              
            else 
				xfpolys(X,Y,C)
			end
			xclip()

			// Now draw the axis and grid (2nd part of process2DAxis)

			if state('nextPlot')=='erase'
				draw2DAxis(state,nTicksX,nTicksY,foreground,background,modeStart,gridFlag,gridColor)
			end
			
			if Xlabel~=" "
			   xlabel(Xlabel);
			end

			if Ylabel~=" "
			   ylabel(Ylabel);
			end
		else
        

            
            if BackFaceCulling=="yes"
            	xset('hidden3d',0);
            else
			    if BackFaceColor=="auto";
				    xset('hidden3d',mean(tab));		
			    elseif BackFaceColor=="none";
				    xset('hidden3d',-1);
                else 
            	    xset('hidden3d',addcolor(BackFaceColor));
			    end            
            end
            
			[modeStart]=process3DPrelim(win,axisVect,axisRatio,axisStyle,colorBar,tab);

			doThePlot3d1(X,Y,list(Z,C),azimuth,elevation,labels,[flag modeStart],axis());
		end

	else

		str=sprintf('mainPlot3d : %s is an unknown type of plot ',typeOfPlot);
		error(str);

	end

end

if state('nextPlot')=='erase'

	if typeOfPlot=='pcolor' | typeOfPlot=='bar'
//		ax(1)=min(X); ax(2)=min(Y);
//		ax(3)=max(X); ax(4)=max(Y);
//		axisVect=[ax(1) ax(3) ax(2) ax(4)];
	else         
		ax(1)=min(X); ax(2)=min(Y); ax(3)=min(real(Z));
		ax(4)=max(X); ax(5)=max(Y); ax(6)=max(real(Z));
		axisVect=[ax(1) ax(4) ax(2) ax(5) ax(3) ax(6)];
		state('axis')=ax;
	end


        if state('subplotState')=='first'
   	 state('subplotState')='other';
        end
end

saveGraphicState(state,win);

if Title~=[]
   title(Title);
end

if matOfLegends ~= []
   processLegend(win,matOfLegends,typeOfLegend);
end


endfunction // end of mainPlot3d
