function bar(varargin)

    argList=varargin;

    barRelativeWidth=0.8;
    argNumber=1;
    minX=%inf;
    maxX=-%inf;
    minY=%inf;
    maxY=-%inf;
    barStyle="group";
    faceColorIndex=0;
    lineStyle=1; marker=[];

    win=xget('window'); // get the number of current graphic window
    state=loadGraphicState(win);

while length(argList)

    if type(argList(1))==15 // If the first element is itself a list 

      tempList=argList(1); // expand this list in the main argument list
      argList(1)=null();
      for i=length(tempList):-1:1
          argList(0)=tempList(i);
      end
    
    end

    if type(argList(1))==1 // If the first argument is a matrix

      if exists('X')
          error('bar : argument %d has not the expected type',argNumber);      	
      end

      if length(argList)==1 // If only one argument

         if or(size(argList(1))==1)  // If this is a vector

            argList(0)=1:length(argList(1)); // insert an abcsissa vector of same length,

         else                                  // if this is a matrix,

            argList(0)=1:size(argList(1),1); // insert an abcsissa vector with 

         end                                   // length = number of lines of 1st argument
         argNumber=argNumber-1;

      else

         if (type(argList(2))==10 | (type(argList(2))==1 & length(argList(2))==1)) 
            if or(size(argList(1))==1) // same treatment as above
               argList(0)=1:length(argList(1));
            else
               argList(0)=1:size(argList(1),1);
            end
            argNumber=argNumber-1;
         end

      end

      if (type(argList(2))==1)  // If the second argument is a matrix

         [X,Y]=checkXYPair('bar',argList(1),argList(2)); // verify (x,y) consistency

         minX=min(minX,min(X));         maxX=max(maxX,max(X));
         minY=min(minY,min(Y));         maxY=max(maxY,max(Y));

         axisVect=[minX maxX minY maxY];

         argList(1)=null(); // Deletion of the two top arguments
         argList(1)=null(); // in the argument list
         argNumber=argNumber+2;

         if length(argList) // If there is a next argument

            if type(argList(1))==1
                  if argList(1)>0 & argList(1)<=2
                      barRelativeWidth=argList(1);
                      argList(1)=null(); // Delete the top argument in the argument list
                      argNumber=argNumber+1;
                  else
                      error('bar : width parameter must be between 0 and 2');                      
                  end
           end
         
         end

      else
              error('bar : argument %d has not the expected type',argNumber);
      end

    elseif (type(argList(1))==10) // If this argument is a string

         select argList(1) // Try to identify a property name
         case 'group'
           argList(1)=null();argNumber=argNumber+1;
           barStyle="group";
         case 'stack'
           if or(Y<0)
           			error('bar : stacked moded is only allowed for positive Y')
           end
           argList(1)=null();argNumber=argNumber+1; 
           barStyle="stack";
         else // Try to identify a colorspec
            [_color,marker,markerSize,lineStyle,fail]=getColorNumber(argList(1));  
            if ~fail
                argList(1)=null();argNumber=argNumber+1;
                faceColorIndex=_color;
            else
                break;
            end
         end // select argList(1)

       else

         error(sprintf('bar : argument %d has not the expected type',argNumber));

       end // if type(argList(1)==10)

    end

    nc=size(Y,2);
    barDX=min(X(2:$)-X(1:$-1))*barRelativeWidth;

    caxis([1 nc]);

    _X=[];_Y=[];_F=[];

    if barStyle=="group"

        axisVect=axisVect+[-barDX barDX 0 0];
        axisVect(3)=min(0,axisVect(3));
        axisVect(4)=max(0,axisVect(4));

        if nc~=1
              barDX=0.8*barDX;
        end

        XP=[-1 1 1 -1]'*barDX/2/nc*barRelativeWidth;

         for k=1:nc
          _X=[_X XP(:,ones(length(X),1))+X(:,ones(4,1))'+barDX/nc*(k-.5-.5*nc)];
          _Y=[_Y [0 0 1 1]'.*.Y(:,k)'];
          _F=[_F k(ones(1,length(X)))];
         end

    else // barStyle="stack"

        axisVect=axisVect+[-barDX barDX 0 0];
        axisVect(3:4)=[0 max(sum(Y,'c'))];
        

         for k=1:length(X)
           XP=[-1 1 1 -1]'*barDX/2+X(k);
           for l=1:nc
             _X=[_X XP];
             _Y=[_Y ([0 0 Y(k,l) Y(k,l)]+sum(Y(k,1:l-1)))'];   
             _F=[_F l];
           end
         end

    end

    if faceColorIndex
       argList(0)=state.colormap(faceColorIndex,:);
       argList(0)='facecolor';
    end

    if length(argList)
       mainPlot3d('bar',list(_X,_Y,_F,'axis',axisVect,argList))
    else
       mainPlot3d('bar',list(_X,_Y,_F,'axis',axisVect))   
    end

    [_1,lims]=xgetech();
    xsegs([lims(1) lims(3)],[0 0]);

    caxis auto

endfunction

