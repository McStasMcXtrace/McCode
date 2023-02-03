function mcdisplay(varargin)
%
% mcdisplay.m:
%
% Implementation of matlab functions for mcdisplay online plotting.
%
% Copyright 2003 Peter Willendrup, RIS�national laboratory
%
% Covers also the scripts
% PlotInstrument.m
% wait.m
% CheckNeutNumber.m
% ReshapeTransform.m
% CheckTimeout.m
% resume.m
%
% Written by: P. Willendrup
% Date: May 13th, 2003
% Release: McStas 1.7
% Origin: Risoe
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

% Check/parse input parameters
if nargin==0
  error('mcdisplay.m: need at least one input parameter...');
else
  task=varargin{1};
  if not(isstr(task))
    error('mcdisplay.m: first input parameter must be a string...');
  else
    switch task
     case 'Init'
     Init;
     case 'PlotInstrument'
      PlotInstrument(varargin{2:length(varargin)});
     case 'Load'
      Load;
     case 'PlotNeutron'
      PlotNeutron(varargin{2:length(varargin)});
     case 'Timeout'
      Timeout;
     case 'Exit'
      % This is where one would implement something to send
      % kill -TERM to calling perl process..
      exit
     otherwise
      error(['mcdisplay.m: task ' task ' unknown...']);
    end
  end
end

function Init
% Initialisation of INSTRUMENT struct
global INSTRUMENT;
INSTRUMENT.name=cell(0);
INSTRUMENT.transformation=[];
INSTRUMENT.coords=[];
INSTRUMENT.Ncnt=0;
INSTRUMENT.stop=0;
INSTRUMENT.read=0;
INSTRUMENT.neut=[];
global component;
global coordinates;
global listcnt;
global compcount;
compcount=0;
global MovieFlag
MovieFlag=0;
global Frames
Frames=100;
global FrameCount;
FrameCount=0;

% Set up simple control window
h=figure('numbertitle','off','name','mcdisplay controls (Trace/3D view)',...
		      'units','normalized','tag','McDisp','toolbar',...
		      'figure','pointer','watch',...
		      'position',[0.1 0.1 0.8 0.8]);

INSTRUMENT.fig=h;

% Define axis extent
axes('position',[0.1 0.2 0.8 0.7]);

% '# Trajectories to keep labels'
uicontrol(h,'style','text','units','normalized','position',...
	  [6/11 0.05 1/11 0.05],'string','Trajectories to keep','fontsize',10);
% 'Exit button'
uicontrol(h,'style','pushbutton','units','normalized','position',...
	  [10/11 0.05 1/11 0.05],'string','Exit','callback', ...
	  'exit;','fontsize',10);

% 'Lock button'
uicontrol(h,'style','pushbutton','units','normalized','position',...
	  [7/11 0 1/11 0.05],'string','Lock','callback', ...
	  'wait(INSTRUMENT.fig);','fontsize',10,'tag','Lock');

% 'Unlock button'
uicontrol(h,'style','pushbutton','units','normalized','position',...
	  [7/11 0.05 1/11 0.05],'string','Unlock','callback', ...
	  'resume(INSTRUMENT.fig);','fontsize',10,'tag','Unlock');

% '# Trajectories to keep edit field'
uicontrol(h,'style','edit','units','normalized','position',...
	  [6/11 0 1/11 0.05],'string','1','userdata',1,'callback', ...
	  'CheckNeutNumber;','fontsize',10,'tag','NumTrace');

% 'Plot / Replot button'
uicontrol(h,'style','pushbutton','units','normalized','position',...
	  [0/11 0 1/11 0.05],'string','Redraw','callback', ...
	  'PlotInstrument;','fontsize',10);

% 'Reset view button'
uicontrol(h,'style','pushbutton','units','normalized','position',...
	  [0/11 0.05 1/11 0.05],'string','Reset view','callback', ...
	  'view([1e-10 90-1e-10]);','fontsize',10);

% 'Timeout label'
uicontrol(h,'style','text','units','normalized','position',...
	  [8/11 0.05 1/11 0.05],'string','Plot timeout (sec)','fontsize',10);

% 'Timeout edit field'
uicontrol(h,'style','edit','units','normalized','position',...
	  [8/11 0 1/11 0.05],'string','0.2','userdata',0.2,'callback', ...
	  'CheckTimeout;','fontsize',10,'tag','Timeout');

% 'Component list label'
uicontrol(h,'style','text','units','normalized','position',...
	  [1/11 0 1/11 0.1],'string','Viewed Comp."s:','fontsize',10);

% 'Start component listbox'
uicontrol(h,'style','listbox','units','normalized','position',...
	  [2/11 0 2/11 0.1],'string','start','tag','startcomp', ...
	  'fontsize',10);

% 'End component listbox'
uicontrol(h,'style','listbox','units','normalized','position',...
	  [4/11 0 2/11 0.1],'string','end','tag','endcomp','fontsize',10);

function Load
%
% Set first/last component on the interface
%
global INSTRUMENT
firstcomp=1;
lastcomp=length(INSTRUMENT.name);
if isfield(INSTRUMENT,'firstcomp')
  for j=1:length(INSTRUMENT.name)
    if strmatch(INSTRUMENT.firstcomp,INSTRUMENT.name{j},'exact')
      firstcomp=j;
    end
  end
end
if isfield(INSTRUMENT,'lastcomp')
  for j=1:length(INSTRUMENT.name)
    if strmatch(INSTRUMENT.lastcomp,INSTRUMENT.name{j},'exact')
      lastcomp=j;
    end
  end
end

set(findobj('tag','startcomp'),'string',INSTRUMENT.name,'value',firstcomp);
set(findobj('tag','endcomp'),'string',INSTRUMENT.name,'value',lastcomp);


function PlotNeutron(x,y,z)
%
% Function to plot / delete neutron rays
%
global INSTRUMENT
h=plot3(x,y,z,'k-','tag','neutron');
h1=plot3(x,y,z,'ro','tag','neutron');
h=[h1 h];
INSTRUMENT.neut=[INSTRUMENT.neut h];
Nmax=2*get(findobj(INSTRUMENT.fig,'tag','NumTrace'),'userdata');
Ndrawn=length(INSTRUMENT.neut);
Nclear=Ndrawn-Nmax;
if Nclear>0
  for j=1:Nclear
    eval('delete(INSTRUMENT.neut(j))','');
  end
  INSTRUMENT.neut(1:Nclear)=[];
end
%if (MovieFlag==1) & (FrameCount<Frames)
%  if isempty(Movie)
%    FrameCount=0;
%  else
%    M=Movie;
%  end
%  FrameCount=FrameCount+1;
%  M(FrameCount)=getframe;
%  Movie=M;
%  save Movie
%end


function Timeout
%
% Do a pause() before reading stream data
%
global INSTRUMENT
pause(get(findobj(INSTRUMENT.fig,'tag','Timeout'),'userdata'));


