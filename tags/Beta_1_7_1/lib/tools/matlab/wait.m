function wait(fig)
%
% Function for setting pointer to 'arrow' + calling uiwait
% 
set(fig,'pointer','arrow');
set(findobj(fig,'type','uicontrol'),'enable','on');
set(findobj(fig,'tag','Lock'),'enable','off');
uiwait(fig);
