function CheckTimeout
%
% Check that timeout value is reasonable
%
str=get(gco,'string');
if isempty(str2num(str)) 
  % Revert to previously entered data
  set(gco,'string',num2str(get(gco,'userdata')))
else
  % Set to 0<=num<=30 (no need to wait more than 30 secs...) 
  num=str2num(str);
  num=max(0,num);
  num=min(num,30);
  set(gco,'string',num2str(num),'userdata',num);
end
