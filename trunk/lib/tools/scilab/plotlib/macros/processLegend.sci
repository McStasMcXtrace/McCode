function processLegend(mat,typ)

numberOfLegends=size(mat,1);

if numberOfLegends==0
	return
end

win=gcw()

savedThickness=xget('thickness');
state=loadGraphicState(win);
listOfPlots=state('listOfPlots');
fact=state('margin');
vp=state('viewport');
ech=state('ech');

x=ech(1);y=ech(2);w=ech(3);h=ech(4);

blanc=' ';
mat=mat+blanc(ones(numberOfLegends,1));

ind=find(mat==' ');
indcol=1:numberOfLegends;
indcol(ind)=[];
mat(ind)=[];

numberOfLegends=length(indcol);

rect=xstringl(1,1,mat);

wb=rect(3);
hb=rect(4);

[wrect,frect,logflag,arect]=xgetech();

D=0.1;

wf=wb/(frect(3)-frect(1))+D;
hf=hb/(frect(4)-frect(2));

m=max([w*D/10 h*D/10]);

select typ
case -1
case 1
   rect=[x+w*(1-wf)-m y+m w*(wf) h*hf]
case 2
   rect=[x+m y+m w*(wf) h*hf]
case 3
   rect=[x+m y+h*(1-hf)-m w*(wf) h*hf]
case 4
   rect=[x+w*(1-wf)-m y+h*(1-hf)-m w*(wf) h*hf]
case 5
   ll=(wf)*(frect(3)-frect(1));
   hh=hb;
   xr=frect(1)+(frect(3)-frect(1))/2;
   yr=frect(2)+(frect(4)-frect(2))/2;
   xset('pattern',addcolor(state('foreground')))
   driver('X11');
   xset('alufunction',6) // xor mode
   xrect(xr,yr,ll,hh);
   rep=[0 0 -1];
   while rep(3)~=0
      rep=xgetmouse();
      xrect(xr,yr,ll,hh);
      xrect(rep(1)-ll/2,rep(2)+hh/2,ll,hh);
      xr=rep(1)-ll/2;
      yr=rep(2)+hh/2;
   end
   xrect(xr,yr,ll,hh);
   xset('alufunction',3); // replace mode
   driver('Rec')
   
   rect=[x+w*(xr-frect(1))/(frect(3)-frect(1)),...
   y+h*(1-(yr-frect(2))/(frect(4)-frect(2))),...
   w*wf h*hf]
end

xsetech(wrect=rect,frect=[0 0 wf 1],arect=[0 0 0 0]);

xset('color',17);

xfrect(0,1,wf,1);
xset('color',addcolor(state('foreground')))
xset('thickness',1);
xset('line style',1);
xrect(0,1,wf,1);
xstring(D,0,mat)

step=1/(numberOfLegends);

for k=1:numberOfLegends

      [col,markerId,markersize,lineStyle]=listOfPlots(indcol(k))(3:6);
      xset('line style',lineStyle);
      xset('thickness',markersize);

      X=[D*0.2 D*0.8]';
      Y=(1-step/2-(k-1)*step)*[ 1 1]';
      Xmark=D/2;
      Ymark=Y(1);

      if markerId ~=[]
         xset('mark',markerId,markersize);
         xset('dashes',col);
         xpoly(Xmark,Ymark,'marks');
         if lineStyle>0
            xset('thickness',markersize);
	    xset('line style',lineStyle);
            xsegs(X,Y)
         end
      else
         xsegs(X,Y,col)
      end
end

xset('thickness',savedThickness);
setCurrentViewport(state,win)
