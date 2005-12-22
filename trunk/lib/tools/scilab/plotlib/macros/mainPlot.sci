function mainPlot(typeOfPlot,argList)

minX=%inf;
maxX=-%inf;
minY=%inf;
maxY=-%inf;

liste=list();
currentColor=1;
argNumber=1;
marker=[];
markerSize=1;
numberOfLegends=0;
matOfLegends=[];
typeOfLegend=[];

axisRatio='auto';
axisTightX=%F;
axisTightY=%F;
axisStyle='normal';
axisVect=[];

ticksx=[];
ticksy=[];

foreground=[];
background=[];

Xlabel=[];
Ylabel=[];
Title=[];

gridFlag=%f;
gridColor=[];

if winsid()==[]
  fig()
end

win=xget('window'); // get the number of current graphic window

xset('auto clear','off')  
savedThickness=xget('thickness');

while length(argList)
   
   if type(argList(1))==15 // If the first element is itself a list 

      tempList=argList(1); // expand this list in the main argument list
      argList(1)=null();
      for i=length(tempList):-1:1
          argList(0)=tempList(i);
      end
   end
   
   if type(argList(1))==1 // If the first argument is a matrix
	
	 if argList(1)==[]
	 	return;
	end
	

      if length(argList)==1 // If only one argument

         if or(size(argList(1))==1)  // If this is a vector

            argList(0)=1:length(argList(1)); // insert an abcsissa vector of same length,

         else                                  // if this is a matrix,

            argList(0)=1:size(argList(1),1); // insert an abcsissa vector with 

         end                                   // length = number of lines of 1st argument
         argNumber=argNumber-1;

      else

         if type(argList(2))==10 
         // If the second argument is a string
            if or(size(argList(1))==1) // same treatment as above
               argList(0)=1:length(argList(1));
            else
               argList(0)=1:size(argList(1),1);
            end
            argNumber=argNumber-1;
         end

      end

      if (type(argList(2))==1 | type(argList(2))==13)  // If the second argument is a matrix or a function

         [X,Y]=checkXYPair(typeOfPlot,argList(1),argList(2)); // verify (x,y) consistency

         minX=min(minX,min(X));         maxX=max(maxX,max(X));
         minY=min(minY,min(Y));         maxY=max(maxY,max(Y));
         
         axisVect=[minX maxX minY maxY];

         argList(1)=null(); // Deletion of the two top arguments
         argList(1)=null(); // in the argument list
         argNumber=argNumber+2;

         _color=0; lineStyle=1; marker=[];

         if length(argList) // If there is a next argument

            if (type(argList(1))==10) // If this argument is a string

               [_color,marker,markerSize,lineStyle,fail]=getColorNumber(argList(1)); 

               if ~fail // the string seems to be a maker/color combination
                 
                 argList(1)=null(); // Delete the top argument in the argument list
                 argNumber=argNumber+1;
               else
                  _color=0; lineStyle=1; marker=[];
               end
            end

         end

         if size(X,2)==1 // If the abscissa is a vector
            for j=1:size(Y,2) // all ordina share this abscissa
               [currentColor,liste]=addPlot(X,Y(:,j),_color,marker,markerSize,lineStyle,win);
           end
         else // Abscissa and ordina are both matrices
            for j=1:size(X,2) // Add each couple
               [currentColor,liste]=addPlot(X(:,j),Y(:,j),_color,marker,markerSize,lineStyle,win);
            end
         end

      end

   elseif (type(argList(1))==10) // If this argument is a string

      select argList(1) // Try to identify a property name

      case 'axis'
         [axisStyle,axisRatio,axisVect,axisTightX,axisTightY] = ...
	 parseAxis(typeOfPlot,argList,axisStyle,axisRatio,axisVect,axisTightX,axisTightY);
	       
	 argList(1)=null(); argList(1)=null();

      case 'ticksX'
         [ticksx] = parseTicks(typeOfPlot,argList);
	       
	 argList(1)=null(); argList(1)=null();

      case 'ticksY'
         [ticksy] = parseTicks(typeOfPlot,argList);
	       
	 argList(1)=null(); argList(1)=null();

      case 'background'
         background = parseColor(typeOfPlot,'background',argList);
         argList(1)=null(); argList(1)=null();

      case 'foreground'
         foreground = parseColor(typeOfPlot,'foreground',argList);
         argList(1)=null(); argList(1)=null();
         
      case 'edgecolor'
         edgecolor = parseColor(typeOfPlot,'edgecolor',argList);
         argList(1)=null(); argList(1)=null();

      case 'facecolor'
         facecolor = parseColor(typeOfPlot,'facecolor',argList);
         argList(1)=null(); argList(1)=null();
 
      case 'Xscale'
         Xscale=parseScale(typeOfPlot,'Xscale',argList);
         argList(1)=null(); argList(1)=null();

      case 'Yscale'
         Yscale=parseScale(typeOfPlot,'Yscale',argList);
         argList(1)=null(); argList(1)=null();

      case 'legend'

         argList(1)=null();
 	 [matOfLegends,nbProc,typeOfLegend]=parseLegend(typeOfPlot,argList,length(liste));         
         if nbProc==0
            error('plot : missing string(s) for legend');
	 end
         for k=1:nbProc; argList(1)=null(); end;

      case 'grid'
      
         [gridFlag,gridColor]=parseGrid(typeOfPlot,argList);
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
      
      else
         error(sprintf('plot : %s is an unknown property name',argList(1)));
      end // select argList(1)

   else
      str=sprintf('plot : argument %d has not the expected type',argNumber);
      error(str);

   end // if type(argList(1))

end // while length(argList)

// Common 2D/3D stuff

[foreground,background]=processSFB(foreground,background,win,typeOfPlot)

// Common 2D stuff

[modeStart,modeAdd,modeScale,nTicksX,nTicksY]=process2DPrelim(win,liste,Xscale,Yscale,...
axisVect,axisTightX,axisTightY,axisRatio,axisStyle,'off',[],matOfLegends,typeOfLegend,ticksx,ticksy)

// Process the list of plots to do

state=loadGraphicState(win);
state.typeOfPlot=typeOfPlot;

//xset('pattern',xget('foreground'));


for k=1:length(liste)

   [X,Y,col,markerId,markerSize,lineStyle]=liste(k)(1:6)

   if k==1 & state('nextPlot')=='erase' // if first (X,Y) pair of the list and 'erase' property on, 
                                        // plot the axis and all the stuff

       process2DAxis(state,nTicksX,nTicksY,foreground,background,modeStart,modeScale,gridFlag,gridColor)

   end

   xset('thickness',markerSize);
   xset('line style',lineStyle);
   if markerId ~=[]
      xset('color',col);
      xset('mark',markerId,markerSize);

      plot2d1(modeScale,X,Y,-markerId,modeAdd,' ',state('axis'));
      if lineStyle>0
         xset('thickness',markerSize);
         xset('line style',lineStyle); 
         plot2d1(modeScale,X,Y,col,modeAdd,' ',state('axis'))
      end
      xset('color',0)
   else
        plot2d1(modeScale,X,Y,col,modeAdd,' ',state('axis')); 
   end
   xset('thickness',savedThickness);
end

// Now draw the axis and grid (2nd part of process2DAxis)

if state('nextPlot')=='erase'
   draw2DAxis(state,nTicksX,nTicksY,foreground,background,modeStart,gridFlag,gridColor)
   if state('subplotState')=='first'
   	state('subplotState')='other';
   end
end

saveGraphicState(state,win);

// Now process the legends (if applicable)

if matOfLegends ~= []
   processLegend(win,matOfLegends,typeOfLegend);
end

if Xlabel~=[]
   xlabel(Xlabel);
end

if Ylabel~=[]
   ylabel(Ylabel);
end

if Title~=[]
   title(Title);
end

xset('foreground',addcolor(foreground));

// end of mainPlot

endfunction
