function wait(fig)
%
% Function for setting pointer to 'arrow' + calling uiwait
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
set(fig,'pointer','arrow');
set(findobj(fig,'type','uicontrol'),'enable','on');
set(findobj(fig,'tag','Lock'),'enable','off');
uiwait(fig);
