function processLegend(win,mat,typ)

numberOfLegends=size(mat,1);

if numberOfLegends==0
	return
end

blanc=' ';
mat=blanc(ones(numberOfLegends,1))+mat;

ind=find(mat==' ');
indcol=1:numberOfLegends;
indcol(ind)=[];
mat(ind)=[];

nl=length(indcol);

if nl==0
	return
end

savedThickness=xget('thickness');
state=loadGraphicState(win);

listOfPlots=state('listOfPlots');
nl=min(nl,length(listOfPlots))
mat=mat(1:nl);

if mat==[]
   return
end

fact=state('margin');
vp=state('viewport');
ech=state('ech');
x=ech(1);y=ech(2);w=ech(3);h=ech(4);


[wrect,frect,logflag,arect]=xgetech();

dim = xget('wdim');
dim = dim.*vp(3:4);

rect=xstringl(1,1,mat,6,setFontSize(vp));
if MSDOS
  hl=rect(4)/(frect(4)-frect(2))*h;
else
  hl=rect(4)/(frect(4)-frect(2))*h*1.5;
end
wl=rect(3)/(frect(3)-frect(1))*w*1.1;

select typ
case -1
case 1
xl=x+w-wl-w/10;
rect=[xl-w/40 y+h/40 wl+w/10 hl];
case 2
rect=[x+w/40 y+h/40 wl+w/10 hl];
case 3
yl=y+h-hl;
rect=[x+w/40 yl-h/40 wl+w/10 hl];
case 4
xl=x+w-wl-w/10;
yl=y+h-hl;
rect=[xl-w/40 yl-h/40 wl+w/10 hl];
case 5
end
xsetech(wrect=rect,frect=[0 0 1 1],arect=[0 0 0 0]);
xset('color',17);
xfrect(0,1,1.02,1)
xset('color',addcolor(state('foreground')))
xset('thickness',1);
xset('line style',1);
xrect(0,1,1.02,1)

for i=1:nl 

      xset('color',addcolor(state('foreground')))
  	   xstringb(0,1-i/nl,mat(i,:),wl/(wl+w/10),1/nl)

      [col,markerId,markersize,lineStyle]=listOfPlots(indcol(i))(3:6);
      
      if state.typeOfPlot=="plot" | ...
         state.typeOfPlot=="semilogx" | ...  
         state.typeOfPlot=="semilogy" | ...
         state.typeOfPlot=="loglog"
      
         xset('line style',lineStyle);
         xset('thickness',markersize);
         step=1/nl;
	       X=linspace(wl/(wl+w/10),0.98,3)';
	       Y=(1-step/2-(i-1)*step)*[ 1 1 1]';
         if markerId ~=[]
           xset('mark',markerId,markersize);
           xset('dashes',col);
   	       XP=linspace(wl/(wl+w/10),0.98,5)';
           xpolys(XP(2:4),Y,-[markerId markerId markerId]);
           if lineStyle>0
             xset('thickness',markersize);
	           xset('line style',lineStyle);
             xpolys(X,Y,col)
           end
         else
           xpolys(X,Y,col)
         end
       elseif state.typeOfPlot=='bar'
         step=1/nl;
	       X=linspace(wl/(wl+w/12),0.97,2)';
	       Y=(1-step/2-(i-1)*step)+[-step/4 step/4]';
	       _X=[X(1) X(2) X(2) X(1) X(1)]';
         _Y=[Y(1) Y(1) Y(2) Y(2) Y(1)]';
         xfpolys(_X,_Y,col);
       end

end

xset('thickness',savedThickness);
setCurrentViewport(state,win);

endfunction
