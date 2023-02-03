function vol=tomo_recon(datadir)
% function vol=tomo_recon(datadir,[filter])
% 
%   Reconstruct a volume dataset from projections created using
%   Tomography.instr (McStas example). datadir is the McStas scan
%   output dir and filter is the filter applied by Matlab iradon
%   (default is 'hann')
%
%   Also shows 3 axis-parallel, central slices from the reconstructed
%   volume.
%
%  PW, 20080620
%

%     This file is part of the McStas neutron ray-trace simulation package
%     Copyright (C) 1997-2004, All rights reserved
%     Risoe National Laborartory, Roskilde, Denmark
%     Institut Laue Langevin, Grenoble, France

%     This program is free software; you can redistribute it and/or modify
%     it under the terms of the GNU General Public License as published by
%     the Free Software Foundation; version 3 of the License.

%     This program is distributed in the hope that it will be useful,
%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%     GNU General Public License for more details.

%     You should have received a copy of the GNU General Public License
%     along with this program; if not, write to the Free Software
%     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

if (isunix == 1)

  if (nargin == 0)
    error('I need at least a datadir')
  end

  if nargin==1
    filter = 'hann';
  end

  if not(exist(datadir) == 7)
    error([datadir ' is not an existing directory'])
  end

  PW=pwd;
  cd(datadir);

  % Ad-hoc read the mcstas.dat
  [tmp,Title]=unix('grep Param: mcstas.dat | grep offfile | cut -f2 -d =');
  Title = ['slice from ' Title ' filtered by ' filter];
  [tmp,Nproj]=unix('grep Numpoints mcstas.dat | cut -f2 -d:');
  Nproj = str2num(Nproj);
  [tmp,range]=unix('grep xlimits mcstas.dat | cut -f2 -d:');
  range = str2num(range);
  Thetas = linspace(range(1),range(2),Nproj);
  
  
  for j=0:(Nproj-1)
    cd(num2str(j));
    unix(['grep -v \# *x_y > mon.dat']);
    load mon.dat
    s=size(mon);
    mon = mon(1:s(1)/3,:);
    [Nslice Nbins]=size(mon);
    Monitors{j+1}=mon(1:Nslice,:)+1;
    unix('rm mon.dat');
    cd('..');
  end
  
  for j=1:Nslice % Slice
    sins = []; % For the sinograms
    for k=1:Nproj % Angle
      chunk = Monitors{k}(j,:);
      % Apply logarithm for mapping of the absorption coeff
      chunk=log(max(chunk(:))./chunk);
      sins = [sins chunk'];
    end
    vol(:,:,j)=iradon(sins,Thetas,filter);                                                                                
  end
  
  % Show central slices from the reconstruction
  
  ijk = ceil(size(vol)./2);
  figure
  subplot (1,3,1);
  imagesc(squeeze(vol(ijk(1),:,:))');
  set(gca,'ydir','normal');
  title(['Central x ' Title]);

  subplot (1,3,2);
  imagesc(squeeze(vol(:,ijk(2),:))');
  title(Title)
  set(gca,'ydir','normal');
  title(['Central y ' Title]);
  
  subplot (1,3,3);
  imagesc(squeeze(vol(:,:,ijk(3))));
  title(Title)
  set(gca,'ydir','normal');
  title(['Central z ' Title]);
  
  cd(PW);

else
  error('Sorry, only unix type platforms supported for now...')
end

