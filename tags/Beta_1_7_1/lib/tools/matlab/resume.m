function resume(fig)
%
% Function for setting watch type pointer + calling uiresume
% 
set(fig,'pointer','watch');
set(findobj(fig,'type','uicontrol'),'enable','off');
set(findobj(fig,'tag','Lock'),'enable','on');
uiresume(fig);
