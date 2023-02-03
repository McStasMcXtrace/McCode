function PlotInstrument(varargin)
%
% Plot the instrument
%
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
global INSTRUMENT
figure(INSTRUMENT.fig);

% Called from mcdisplay.pl or from uicontrol?
if nargin==1
  Myview=[1e-10 90-1e-10];
  % Axis labels
  xlabel('z/[m]');
  ylabel('x/[m]');
  zlabel('y/[m]');
  title(INSTRUMENT.descr,'interpreter','none');
  set(INSTRUMENT.fig,'Name',[ 'mcdisplay: ' INSTRUMENT.descr ' (Trace/3D view)' ]);
else
  [AZ,EL] = view;
  Myview=[AZ EL];
end
% Be sure to keep what ever is plotted
hold on

% Remove old plot
delete(findobj('tag','Instrument'));

% Cyclic color definition
colors=['y','m','c','g','b'];
cidx=0;

% For the centre coordinates of each component
centres=[];

% "Start out" axis definition
ax=[Inf -Inf Inf -Inf Inf -Inf];

% Get first and last component to render
firstcomp=get(findobj('tag','startcomp'),'string');
firstcomp=firstcomp{get(findobj('tag','startcomp'),'value')};
lastcomp=get(findobj('tag','endcomp'),'string');
lastcomp=lastcomp{get(findobj('tag','endcomp'),'value')};

% Set viewpoint
view(Myview);

% Axis labels
xlabel('z/[m]');
ylabel('x/[m]');
zlabel('y/[m]');
title(INSTRUMENT.descr,'interpreter','none');
set(INSTRUMENT.fig,'Name',[ 'mcdisplay: ' INSTRUMENT.descr ' (Trace/3D view)' ]);
% Run through the component list
Pcount=0;
Plist=cell(0);
% Plot window setup
for j=1:size(INSTRUMENT.name,2)
  if strmatch(INSTRUMENT.name{j},firstcomp,'exact')
    firstcomp=[];
  end
  % First component reached?
  if isempty(firstcomp)
    % Get component data
    cidx=cidx+1;
    col=colors(mod(cidx,size(colors,2))+1);
    component=INSTRUMENT.name{j};
    eval(['T=INSTRUMENT.' component '.T;']);
    centres=[centres T(:,4)];
    eval(['siz=length(INSTRUMENT.' component '.K);']);
    clear Lines;
    if siz>1
      eval(['Lines=INSTRUMENT.' component '.K;']);
    else
      Lines=cell(0);
      eval(['tmp=INSTRUMENT.' component ';']);
      if isfield(tmp,'K') & not(isempty(tmp.K))
        eval(['Lines(1)=INSTRUMENT.' component '.K(1);']);
      end
    end
    % Run through all line pieces of the component
    for k=1:length(Lines)
      xyz=T*Lines{k};
      x=xyz(3,:);
      y=xyz(1,:);
      z=xyz(2,:);
      % Set name label on each component part
      popup=uicontextmenu;
      label=uimenu(popup,'label',component,'ForeGroundColor',col);
      hc=plot3(xyz(3,:),xyz(1,:),xyz(2,:),col,'uicontextmenu',popup);
      set(hc,'tag','Instrument');

      % Determine the full extent of the instrument
      ax(1)=min([ax(1); x(:)]);
      ax(2)=max([ax(2); x(:)]);
      ax(3)=min([ax(3); y(:)]);
      ax(4)=max([ax(4); y(:)]);
      ax(5)=min([ax(5); z(:)]);
      ax(6)=max([ax(6); z(:)]);
    end
  end
  % Last component reached, do nothing more
  if strmatch(INSTRUMENT.name{j},lastcomp,'exact')
    firstcomp=lastcomp;
  end
end

% Set the axis
axis equal;
axis(ax);
ax=axis;
xx=ax(3:4);
yy=ax(5:6);
zz=ax(1:2);
% Only plot viewable component centres
idx=centres(1,:)>=xx(1) & centres(1,:)<=xx(2) & centres(2,:)>=yy(1) & centres(2,:)<=yy(2) & centres(3,:)>=zz(1) & centres(3,:)<=zz(2);
c1=centres(1,idx);
c2=centres(2,idx);
c3=centres(3,idx);
centres=[c1;c2;c3];
% Set the 'Instrument' tag on the centre line
hc=plot3(centres(3,:),centres(1,:),centres(2,:),'r-','tag','Instrument');
set(hc,'tag','Instrument');
