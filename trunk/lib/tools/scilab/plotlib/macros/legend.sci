function legend(varargin)

win=gcw();
state=loadGraphicState(gcw());
listOfPlots=state('listOfPlots');
nb=length(listOfPlots);
parseList=varargin;

[mat,nbproc,typ]=parseLegend('legend',parseList,nb);

if typ==-1
   error('legend : for type=-1 use the property ''legend'' in a plot command');
end

processLegend(win,mat);

endfunction
