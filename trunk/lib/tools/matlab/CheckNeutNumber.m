function CheckNeutNumber
%
% Check that neutron ray number entered is reasonable
%
str=get(gco,'string');
if isempty(str2num(str))
  % Revert to previously entered data
  set(gco,'string',num2str(get(gco,'userdata')))
else
  % Is a number, set to integer + positive
  set(gco,'userdata',abs(round(str2num(str))));
end

