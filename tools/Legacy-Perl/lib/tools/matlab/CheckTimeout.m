function CheckTimeout
%
% Check that timeout value is reasonable
%
%   This file is part of the McStas neutron ray-trace simulation package
%   Copyright (C) 1997-2004, All rights reserved
%   Risoe National Laborartory, Roskilde, Denmark
%   Institut Laue Langevin, Grenoble, France
%
%   This program is free software; you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation; version 3 of the License.
%
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
%
%   You should have received a copy of the GNU General Public License
%   along with this program; if not, write to the Free Software
%   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
