function [cmap]=loadDefaultColormap()

state=loadGlobalGraphicState();
cmap=state('defaultColormap');


endfunction
