function data = mcplot(varargin)
% mcplot: plot a McCode simulation result
% data = mcplot(filename, ...)
%
% This function displays a McCode simulation result a single window with 
% subplots. It also returns the McStas simulation structures. 
%
% input:
%  filename: one or more simulation name(s) or directory
%          or a single detector file name
%          if filename does not exist, a file selector is called.
%  optional string: -png, -eps, -fig, -pdf, -jpg 
%            will export further figures directly to files
% 
% output:
%  data: a cell of detector structures
%
% examples:
%   mcplot
%   mcplot mcstas.sim
%   mcplot -png simulation
%
% Written by: E. Farhi
% Date: April 16st 2010
% Release: McStas 1.6
% Version: $Revision: 1.229 $
% Origin: ILL
%
%   This file is part of the McCode neutron/X ray-trace simulation package
%   Copyright (C) 1997-2010, All rights reserved
%   Risoe National Laborartory, Roskilde, Denmark
%   Institut Laue Langevin, Grenoble, France
%
%   This program is free software; you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation; version 2 of the License.
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

% check for input argument: filename ?
  data = {};
  
  if nargin == 0
    filename = '';
  elseif nargin == 1
    filename = varargin{1};
  else
    save_as = '';
    index_out=0;
    for index= 1 : size(varargin,2)
      this_arg = varargin{index};
      if strncmp(this_arg, '-', 1)  % get option for export 
        save_as=this_arg; 
        continue;
      else index_out = index_out+1; end
      this_file = varargin{index};
      this_data = mcplot(this_file);
      data{index_out} = this_data;
      
      % check for direct export options
      if ~isempty(save_as)
        set(gcf, 'Visible','off');
        switch save_as
        case '-png'
          saveas(gcf, [ this_file '.png' ],'png'); disp(['Exported as ' this_file '.png']);
        case '-jpg'
          saveas(gcf, [ this_file '.jpg' ],'jpg'); disp(['Exported as ' this_file '.jpg']);
        case '-fig'
          saveas(gcf, [ this_file '.fig' ],'fig'); disp(['Exported as ' this_file '.fig']);
        case '-eps'
          saveas(gcf, [ this_file '.eps' ],'eps'); disp(['Exported as ' this_file '.eps']);
        case '-pdf'
          saveas(gcf, [ this_file '.pdf' ],'pdf'); disp(['Exported as ' this_file '.pdf']);
        otherwise
          save_as = '';
        end
        if ~isempty(save_as), close(gcf); end
      end
    end
    if length(data) == 1, data=data{1}; end
    return
  end

  if isempty(filename)
    [filename, pathname] = uigetfile('*.*', 'Select data file to load');
    if isempty(filename), return; end
    filename = fullfile(pathname, filename);
  end
  
  % import data set
  data = mcplot_load(filename);
  
  % plot (overview or single)
  data = mcplot_display(data);

end % mcplot (main)

% ==============================================================================
% inline functions
% ==============================================================================

% function match= mcplot_filestrfind(filename, pattern, {buffer})
% function match= mcplot_filefgetl(filename, positions)
% function data = mcplot_load(filename)
% function data = mcplot_load_data(filename)
% function data = mcplot_load_matlab(filename)
% function data = mcplot_load_structure(s)
% function data = mcplot_load_sim(filename)
% function data = mcplot_load_scan(filename)
% function data = mcplot_split_multiarray(structure)
% function mcplot_display(structs) 

% ==============================================================================

function match = mcplot_filestrfind(filename, string, buffer)
  % mcplot_filestrfind: find matches of 'string' in the specified filename
  % filename: name of file to search into
  %   string: pattern to search for
  %   buffer: size of buffer used to parse file. buffer=0 will read the entire file
  %           a finite buffer size will only read that amount within the file,
  %           corresponding e.g. with the header.
  %   RETURN: positions of matches within the file
  
  match = [];
  
  if nargin < 2, return; end
  if nargin < 3, 
    buffer = Inf; % will read whole file iteratively
  end
  if buffer == Inf, buffer   = 0;      readToEOF= 1; else readToEOF = 0; end
  if ~buffer,       buffer   = 10000;  end  % default buffer size
  
  invalid         = find(~isstrprop(string, 'print'));
  string(invalid) = ' ';  % replace non printable characters with spaces
  
  if iscellstr(filename), filename = filename{1}; end
  
  fid = fopen(filename);
  if fid == -1, return; end   % also returns when filename is empty
  
  offset = 0; % offset at which the buffer must be loaded from file
  
  % read file by blocks, once or iteratively
  while (~feof(fid) && isempty(ferror(fid))) 
    block = fread(fid, buffer, 'uint8=>char');
    block = block(:)';
    if ~isempty(block)
      invalid        = find(~isstrprop(block, 'print'));
      block(invalid) = ' '; % replace non printable characters with spaces
      
      % find search string in the block
      this_match = strfind(block, string);      % search string
      if ~isempty(this_match)
        match = [ match (this_match+offset) ];  % append new match locations (absolute)
      end
      
      % prepare new offset for iterative search
      offset = ftell(fid);
      if offset == -1, readToEOF = 0; end
    else 
      readToEOF = 0; 
    end
    if readToEOF == 0, break; end
  end % while (readToEOF)
  
  fclose(fid);  % close file
  
end % mcplot_filestrfind

% ==============================================================================

function match = mcplot_filefgetl(filename, positions)
  % mcplot_filefgetl: extract iteratively lines at given positions in file
  %  filename: name of file to search into
  % positions: positions of matches within the file
  % RETURN: cellstr of lines corresponding to positions in file
  
  match = {};
  
  fid = fopen(filename);
  if fid == -1, return; end % also returns when filename is empty
  
  match = cell(1,length(positions));
  
  for index=1:length(positions)
    fseek(fid, positions(index), -1);  % jump at match position
    line = fgetl(fid);
    if ~ischar(line), break; end
    match{index} = strtrim(line);
  end
  
  fclose(fid);  % close file
end % mcplot_filefgetl

% ==============================================================================

function data=mcplot_load(filename)
  % mcplot_load: loads data from directory or file name
  %   Handles McCode and Matlab format, as overview (.sim), directory and single data file
  % filename: name of file to load, or a structure
  %   RETURN: a single structure or a cell array of data structures
  
  data = {};
  
  if ishandle(filename)
    data = get(filename, 'UserData');
  elseif isstruct(filename)
    [data,parameters] = mcplot_load_structure(filename);
  elseif iscellstr(filename)
    for index=1:length(filename)
      data = { data{:} mcplot_load(filename{index}) };
    end
    return
  elseif isdir(filename)  % search for mcstas.sim, mcstas.dat and content.sim in directory
    if ~isempty(     dir(fullfile(filename, 'mcstas.sim'))) % original mcstas files
      data = mcplot_load(fullfile(filename, 'mcstas.sim'));
    elseif ~isempty( dir(fullfile(filename, 'mcstas.dat')))
      data = mcplot_load(fullfile(filename, 'mcstas.dat'));
    elseif ~isempty( dir(fullfile(filename, 'mccode.sim'))) % common mcstas/mcxtrace files
      data = mcplot_load(fullfile(filename, 'mccode.sim'));
    elseif ~isempty( dir(fullfile(filename, 'mccode.dat')))
      data = mcplot_load(fullfile(filename, 'mccode.dat'));
    elseif ~isempty( dir(fullfile(filename, 'mcstas.m')))   % matlab related files
      data = mcplot_load(fullfile(filename, 'mcstas.m'));
    elseif ~isempty( dir(fullfile(filename, 'mccode.m')))
      data = mcplot_load(fullfile(filename, 'mccode.m'));
    end
    % else get all files in directory
  end
  if isempty(data)
  
    this_dir = dir(filename);           % get file reference (and test for existence)
    if isempty(this_dir), return; end   % invalid. exit. Also returns when filename is empty
    
    filepath =fileparts(filename);      % determine directory
    if isempty(filepath), filepath = pwd; end
    
    % find directory entries that are not directories, and non NULL
    index    = find(real([this_dir.isdir]) == 0 & real([this_dir.bytes]) > 0);
    this_dir = char(this_dir.name);
    this_dir = (this_dir(index,:));
    rdir     = cellstr(this_dir);
    rdir     = strcat([ filepath filesep ], char(rdir));
    filename = cellstr(rdir);           % directory listing as cell (names only)

    % filename is now a cellstr: IMPORT HERE
    for index=1:length(filename)
      % test if this is a sim (contains 'begin instrument') or multiarray_1d file
      % in this case, the returned value is a full data set
      this_data = mcplot_load_sim(filename{index});       % load McCode SIM (overview) files
      if isempty(this_data)
        this_data = mcplot_load_scan(filename{index});    % load McCode scan files (multiarray_1d)
      end
      if isempty(this_data)
        this_data = mcplot_load_matlab(filename{index});  % load Matlab format files
      end
      if isempty(this_data)
        this_data = mcplot_load_data(filename{index});    % load single McCode monitor files
      end
      data = { data{:} this_data };
    end
    
  end
  % in case result is embedded inside a cell, we extract it
  if iscell(data) && iscell(data{1}) && length(data) == 1
    data = data{1};
  end
  if iscell(data) && length(data) == 1
    data = data{1};
  end

end % mcplot_load

% ==============================================================================

function structure=mcplot_load_data(filename)
  % mcplot_load_data: load a single data set (not sim nor multiarray) or a structure
  % filename: name of file to load
  %   RETURN: a cell array of data structures
  
  structure = [];
  
  if ischar(filename)
  
    % with scilab, a call to fscanfMat will extract data and header
    fid = fopen(filename);
    if fid == -1, return; end % also returns when filename is empty
    
    % read header
    header = textscan(fid,'#%s','endOfLine','\n','delimiter','\n');
    if iscellstr(header{1}) & length(header)==1
      header = header{1};
    end
    
    paramstr = '';
    % build structure from header fields
    for index=1:length(header)
      line = strread(header{index},'%s','delimiter',':'); % field:value
      if length(line) < 2, continue; end
      value = strtrim(strcat(line{2:end})); value=strrep(value,'''','');
      num   = str2num(value);
      if ~isempty(num), value = num; end
      if strncmp(line{1},'Instrument', length('Instrument'))
        field = 'Instrument';
      else
        field = genvarname(line{1});                      % validate variable name
      end
      if isempty(strfind(field, 'Param'))
        structure = setfield(structure, field, value);    % set new field
      else  % special case for parameters. Build 'Param' sub structure
        if ~isfield(structure,'Param'), structure = setfield(structure, 'Param',[]); end  % create if needed
        param        = getfield(structure, 'Param');
        [var,value]  = strread(value,'%s %s','delimiter','=');  % get 'parameter=value' pair
        value        = strcat(value{1:end}); var = var{1};
        paramstr = [ paramstr var '=' value '; ' ];
        num          = str2num(value);
        if ~isempty(num), value = num; end
        param        = setfield(param, var, value);
        structure    = setfield(structure, 'Param', param);     % store updated sub-structure
      end
    end
    structure = setfield(structure, 'Parameters', paramstr);
    structure.filename = filename;
  elseif isstruct(filename)
    structure=filename;
  end
  
  % extract type and theoretical dimension of data block
  if isfield(structure,'type')
    [t s]=strread(structure.type, '%s %d','delimiter','() ');
    structure.type = t{1};
    structure.size = s(:)';
  else
    return;
  end
  
  % additional compatibility checks (from McCode/Matlab format)
  if isfield(structure, 'parameters') && ~isfield(structure,'Param')
    param = getfield(structure,'parameters');
    structure = rmfield(structure,'parameters');
    structure = setfield(structure, 'Param', param);
  end
  if ~isfield(structure,'Param')
    structure = setfield(structure, 'Param', 'Unknown instrument parameters');
  end
  
  if ischar(filename)
    % load data block
    frewind(fid);
    data     = textscan(fid,'%f','CommentStyle','#');
    fclose(fid);
    if iscell(data) & length(data)==1
      data = data{1};
    end
  elseif isfield(structure, 'data')
    data = structure.data;
  else data=[]; end
  
  if ~isfield(structure, 'size')
    structure.size = size(data);
  end
  if ~isfield(structure, 'Source')
    if isfield(structure,'Instrument')
      structure.Source = structure.Instrument;
    else
      structure.Source = filename;
    end
  end
  if ~isfield(structure, 'component')
    structure.component = 'unknown';
  end
  if ~isfield(structure, 'filename')
    structure.filename = 'unknown';
  end

  if isempty(data), return; end

  % handle multiarray dimension (number of scan steps)
  if length(structure.size) == 1
    n = length(data)/structure.size;
    m = structure.size;
    structure.size = [m n];
  end

  if ischar(filename)
    % reshape data block from 'type'
    len      = prod(structure.size);
    signal   = transpose(reshape(data(1:len), fliplr(structure.size)));
    if prod(size(data)) >= 2*len
      errors = transpose(reshape(data((len+1):(2*len)), fliplr(structure.size)));
    else 
      errors = []; 
    end
    if prod(size(data)) >= 3*len
      events = transpose(reshape(data((2*len+1):(3*len)), fliplr(structure.size)));
    else 
      events = []; 
    end
    % add data block
    structure.data     = signal;
    structure.errors   = errors;
    structure.events   = events;
  end

end % mcplot_load_data

% ==============================================================================

function data=mcplot_load_matlab(filename)
  % mcplot_load_matlab: test and open a McCode/Matlab data file and load its content
  % filename: name of file to load (matlab format)
  %   RETURN: a data structure, or empty if not a McCode/Matlab file
  
  data = {};
  
  % search for 'Matlab' in header. Return if not found
  isMatlabScript = [ mcplot_filestrfind(filename, 'Matlab', 10000) ...
                     mcplot_filestrfind(filename, 'matlab', 10000) ...
                     mcplot_filestrfind(filename, '% Embedded function for building', 10000) ];
  
  if isempty(isMatlabScript), return; end % also returns when filename is empty
  
  % in principle, McCode/Matlab data format contains is own 'mcload_inline' routine
  % make sure we get a Matlab .m file
  cur_dir = pwd;
  [pathname, object, ext]=fileparts(filename);
  if isempty(pathname), pathname=pwd; end
  object = genvarname(object);
  % create a local copy with right extension (removed afterwards)
  if ~strcmp(filename, fullfile(pathname, [ object '.m' ]))
    copyfile(filename, fullfile(pathname, [ object '.m' ]))
  end
  cd(pathname);  % go in directory where data is
  
  % evaluate Matlab format data file
  data = eval(object, '[]');
  if ~strcmp(filename, fullfile(pathname, [ object '.m' ]))
    delete([ object '.m' ]);
  end
  cd(cur_dir);
  data.filename = filename;
  
  [data, parameters] = mcplot_load_structure(data);   % extract monitors recursively
  
  % insert extracted parameteres in each monitor structure
  if isstruct(data), 
    data.Param=parameters;
  elseif iscell(data)
    for index=1:length(data)
      this_data = data{index};
      if isstruct(this_data), 
        this_data.Param=parameters; data{index} = this_data; 
      end
    end
  end
  
end % mcplot_load_matlab

% ==============================================================================

function [data,parameters]=mcplot_load_structure(s,parameters)
  % mcplot_load_structure: load data from a structure recursively
  %         s: structure with possibly full hierachy. We then search for those that contain some 'data'
  %    RETURN: a single structure or a cell array of data structures
  
  if nargin<2,
    parameters=[];
  end
  data = {};
  
  if ~isstruct(s), return; end
  
  if isfield(s,'data') & isnumeric(s.data)                  % found 'data': we keep that
    data = s;
    data = mcplot_load_data(data);      % check structure
    data.Param = parameters;
  elseif isfield(s,'name') && strcmp(s.name, 'parameters')
    parameters = s;
  else
    tag_names = fieldnames(s);
    for index=1:length(tag_names)       % scan recursively structure fields
      d = getfield(s,tag_names{index});
      if isstruct(d)
        [this_data,parameters] = mcplot_load_structure(d,parameters);
        if ~isempty(this_data)
          if iscell(this_data) && length(this_data) == 1 && isstruct(this_data{1})
            this_data = this_data{1};
          end
          data = { data{:} this_data };
        end
      end
    end
  end
end % mcplot_load_structure
  
% ==============================================================================

function data=mcplot_load_sim(filename)
  % mcplot_load_sim: test and open a .sim (overview) McCode data file and load its content
  % filename: name of file to load
  %   RETURN: a cell array of data structures, or empty if not a scan file
  
  data = {};
  
  % search for 'begin instrument' in header. Return if not found
  isSimFile    = mcplot_filestrfind(filename, 'begin instrument', 10000);
  if isempty(isSimFile)
    return  % also returns when filename is empty
  end
  
  % search for 'filename:' tags, and extract links to other files
  filenameReferences = mcplot_filestrfind(filename, 'filename:');
  filenameLines      = mcplot_filefgetl  (filename, filenameReferences+length('filename:'));
  filepath = fileparts(filename);

  % loop on filenames
  for index = 1:length(filenameLines)
    % calls mcplot_load_data
    this_filename = fullfile(filepath, filenameLines{index});
    this_data     = mcplot_load_data(this_filename);
    if strcmp(this_data.type,'multiarray_1d')
      this_data = mcplot_split_multiarray(this_data);
    end
    data = { data{:} this_data }; % IMPORT sim file references
  end

end % mcplot_load_sim

% ==============================================================================

function data=mcplot_load_scan(filename)
  % mcplot_load_scan: test and open a multiarray (scan) McCode data file and load its content
  % filename: name of file to load
  %   RETURN: a cell array of data structures, or empty if not a sim/scan file
  
  data = {};
  
  % search for 'multiarray_1d' in header. Return if not found
  isMultiArray = mcplot_filestrfind(filename, 'multiarray_1d',    10000);
  if isempty(isMultiArray)  
    return  % also returns when filename is empty
  end
  
  % % IMPORT multiarray file
  data = mcplot_load_data(filename);                      

  % then extract columns in a loop
  data = mcplot_split_multiarray(data);
  
end % mcplot_load_scan

% ==============================================================================

function data = mcplot_split_multiarray(structure)
  % mcplot_split_multiarray: load a multiarray data set and generate monitor column structures
  % structure: single data set structure (multiarray type)
  %    RETURN: a cell array of data structures, one for each monitor in the multiarray
  
  data = {};
  
  % first check if this is a multiarray
  if ~strcmp(structure.type,'multiarray_1d'), return; end
  
  % extract all column labels (each word is reversed to ease _I and _ERR search)
  column_labels = flipud(strread(fliplr(structure.variables),'%s'));  % reverse string so that _I and _ERR start words
  
  % get indices that refer to monitor Intensity and Error bars
  monitor_I     = strmatch(fliplr('_I'), column_labels);    % _I
  monitor_ERR   = sort([ strmatch(fliplr('_ERR'), column_labels) strmatch(fliplr('_Err'), column_labels) ]);  % _ERR

  % check consistency
  if (length(monitor_I) ~= length(monitor_ERR))
    warning('McPlot:ScanFormatError', ...
      'File: %s: Found %d Monitor intensity, and %d corresponding error entries', ...
      structure.filename, length(monitor_I), length(monitor_ERR));
  end
  
  % get indices that refer to scan variable parameters (i.e. not monitors)
  scan_labels  = setdiff(1:length(column_labels), [ monitor_I ; monitor_ERR ]);
  xvars        = strmatch(fliplr(strtok(structure.xvars)), column_labels, 'exact');
  if isempty(xvars)
    warning('McPlot:ScanFormatError', ...
      'File: %s: Can not find scanned variable ''%s'' within columns\n%s', ...
      structure.filename, structure.xvars, structure.variables);
    xvars = transpose(linspace(structure.xlimits(1), structure.xlimits(2), structure.size(1)));
  else
    xvars          = structure.data(:,xvars);
  end
  % gerenate monitor entries from the scanned data
  for index=1:length(monitor_I)
    this_monitor = fliplr(column_labels{monitor_I(index)});
    this_data = structure;   % initiate single monitor data set from the scan structure
    this_data.type = 'array_1d';
    this_data.size = [ structure.size(1) 4 ];
    this_data.title= [ structure.title ':' this_monitor ];
    this_data.component=this_monitor;
    
    intensity      = structure.data(:,monitor_I(index));
    errors         = structure.data(:,monitor_ERR(index));
    events         = ones(size(intensity));
    this_data.data = [ xvars intensity errors events ];
    data = { data{:} this_data };
  end
end % mcplot_split_multiarray

% ==============================================================================

function data = mcplot_display(data, fig) 
  % opens a new window, with subplots and plots each structure in axes
  % data: single monitor structure or a cell array of monitor structures
  %  fig: optional figure handle to use, else a new figure is created.
  %  RETURN: updated data sets (with plot handle)
  
  h=[];
  
  if nargin < 2, fig = []; end
  if fig == 0,   fig = []; end
  if isempty(fig), fig = figure; end  % create a new figure if required
  
  if iscell(data)
    m = floor(sqrt(length(data)));
    n = ceil(length(data)/m);
    for index=1:length(data)
      subplot(m,n, index);
      data{index} =  mcplot_display(data{index}, fig);
    end
    return
  end
  
  % handle a single plot (a structure)
  if isempty(data), return; end
  if ~isfield(data,'title'), return; end
  set(fig, 'Name', [ data.title ':' data.filename ]);
  S = data.size;
  if isfield(data,'xylimits')
    l = data.xylimits; 
  else
    l = data.xlimits;
  end
  if ischar(l), l=str2num(l); end
  if strcmp(data.type,'array_2d')
    % builds the XY axes from limits
    if ~isfield(data,'x')
      if S(2) > 1, data.stepx=abs(l(1)-l(2))/(S(2)-1); 
      else         data.stepx=0; end
      data.x=linspace(l(1)+data.stepx/2,l(2)-data.stepx/2,S(2));
    end
    if ~isfield(data,'y')
      if S(1) > 1, data.stepy=abs(l(3)-l(4))/(S(1)-1); 
      else         data.stepy=0; end
      data.y=linspace(l(3)+data.stepy/2,l(4)-data.stepy/2,S(1));
    end
    % plot the data as a surface
    z=data.data;
    h=surface(data.x, data.y, data.data);
    xlim([l(1) l(2)]); ylim([l(3) l(4)]);
    shading flat;
  elseif strcmp(data.type,'array_1d')
    if ~isfield(data,'x') || (isfield(data,'x') && length(data.x) ~= size(data.data,1))
      if size(data.data,2) == 1
        % builds the X axis from limits
        if strfind(data.title, 'Scan of')
          % check if this is a scan
          data.x=linspace(l(1),l(2),prod(S));
        else
          %  or normal bin-centered monitor
          if max(S) > 1, data.stepx=abs(l(1)-l(2))/(max(S)-1);
          else           data.stepx=0; end
          data.x=linspace(l(1)+data.stepx/2,l(2)-data.stepx/2,prod(S));
        end
      else
        data.x     =data.data(:,1);
      end
    end
    if size(data.data,2) > 1 & size(data.errors,2) == 0
      if size(data.data,2) >= 4, data.events=data.data(:,4); end
      if size(data.data,2) >= 3, data.errors=data.data(:,3); end
      data.data  =data.data(:,2);
    end
    % plot the data as a curve with error bars or single curve
    if isfield(data, 'errors') && ~isempty(data.errors)
      h=errorbar(data.x, data.data, data.errors);
    else
      h=plot(data.x, data.data);
    end
    xlim([l(1) l(2)]);
  end
  % add labels, title
  t=title({ data.title, data.filename }); 
  set(t,'interpreter','none','fontweight','bold');
  xlabel(data.xlabel);
  ylabel(data.ylabel);
  grid on;
  % store data into plot UserData
  set(h, 'UserData', data, 'Tag',[ 'mcplot_data_' data.filename ]);
  
  % add contextual menu
  hm = uicontextmenu;
  uimenu(hm, 'Label',['About ' data.filename ],'Callback', ...
    [ 'h=get(gco,''userdata''); t=evalc(''data=h''); p=evalc(''parameters=h.Param'',[]); msgbox([ h.title '': '' h.filename, t, p ], [ h.title '': '' h.filename ],''help'');' ])
    
  uimenu(hm, 'Label', [ 'Source:    '    data.Source ]);
  uimenu(hm, 'Label', [ 'Component: '    data.component ]);
  uimenu(hm, 'Label', [ 'Filename:  '    data.filename ]);
  uimenu(hm, 'Separator','on','Label','Toggle grid', 'Callback','grid');
  if strcmp(data.type,'array_1d')
    uimenu(hm, 'Label','Toggle error bars', 'Callback','tmp_h=get(gco,''children''); if strcmp(get(tmp_h(2),''visible''),''off''), tmp_v=''on''; else tmp_v=''off''; end; set(tmp_h(2),''visible'',tmp_v); clear tmp_h tmp_v');
    uimenu(hm, 'Label','Linear/Log scale','Callback', 'if strcmp(get(gca,''yscale''),''linear'')  set(gca,''yscale'',''log''); else set(gca,''yscale'',''linear''); end');
  else
    uimenu(hm, 'Label','Reset Flat/3D View', 'Callback','[tmp_a,tmp_e]=view; if (tmp_a==0 & tmp_e==90) view(3); else view(2); end; clear tmp_a, tmp_e; lighting none;alpha(1);shading flat;rotate3d off;axis tight;');
    uimenu(hm, 'Label','Smooth View','Callback', 'shading interp;');
    uimenu(hm, 'Label','Add Light','Callback', 'light;lighting phong;');
    uimenu(hm, 'Label','Transparency','Callback', 'alpha(0.7);');
    uimenu(hm, 'Label','Linear/Log scale','Callback', 'if strcmp(get(gca,''zscale''),''linear'')  set(gca,''zscale'',''log''); else set(gca,''zscale'',''linear''); end');
  end
  
  
  % export menu items
  uimenu(hm, 'Label',['Duplicate ' data.component ' view'], 'Callback', ...
    [ 'h=get(gco,''userdata''); h.filename = ''' data.component '''; mcplot(h);' ]);
  if ~isempty(dir(data.filename))
    uimenu(hm, 'Label',['Edit ' data.filename], 'Callback', ...
      [ 'h=get(gco,''userdata''); if ~isempty(dir(h.filename)), edit(h.filename); end' ]);
  end
  if ~isdeployed
    uimenu(hm, 'Label',['Export into ' genvarname(data.filename) ],'Callback', ...
      [ 'evalin(''base'',''' genvarname(data.filename) ' = get(gco,''''userdata''''); disp([''''Exported data into variable ' genvarname(data.filename) ''''']);'');'])
  end
    
  uimenu(hm, 'Label',['Save as ' data.filename '.png'], 'Callback', [ 'saveas(gcf, ''' data.filename '.png'',''png''); disp(''Exported as ' data.filename '.png'')' ]);
  uimenu(hm, 'Label',['Save as ' data.filename '.jpg'], 'Callback', [ 'saveas(gcf, ''' data.filename '.jpg'',''jpg''); disp(''Exported as ' data.filename '.jpg'')' ]);
  uimenu(hm, 'Label',['Save as ' data.filename '.fig (Matlab)'], 'Callback',[ 'saveas(gcf, ''' data.filename '.fig'',''fig''); disp(''Exported as ' data.filename '.fig'')' ]);
  uimenu(hm, 'Label',['Save as ' data.filename '.eps'], 'Callback',[ 'saveas(gcf, ''' data.filename '.eps'',''eps''); disp(''Exported as ' data.filename '.eps'')' ]);
  uimenu(hm, 'Label',['Save as ' data.filename '.pdf'], 'Callback',[ 'saveas(gcf, ''' data.filename '.pdf'',''pdf''); disp(''Exported as ' data.filename '.pdf'')' ]);
 
  % add rotate/pan/zoom tools in case java machine is not started
  if ~usejava('jvm')
    uimenu(hm, 'Separator','on','Label','Zoom on/off', 'Callback','zoom');
    uimenu(hm, 'Label','Pan (move)', 'Callback','pan');
    set(gcf, 'KeyPressFcn', @(src,evnt) eval('if lower(evnt.Character)==''r'', lighting none;alpha(1);shading flat;axis tight;rotate3d off;; end') );
    if strcmp(data.type,'array_2d')
      uimenu(hm, 'Label', 'Rotate', 'Callback','rotate3d on');
    end
    uimenu(hm, 'Label','Print ...', 'Callback', 'printdlg(gcf)');
  end
  uimenu(hm, 'Separator','on', 'Label', 'About McCode/McPlot', 'Callback','msgbox({''MCPLOT a Tool to display McCode data set'',''E.Farhi, ILL 2010 <www.mccode.org>''},''About McCode/mcplot'',''help'')');
  
  set(h, 'UIContextMenu', hm);

  % update data structure
  data.handle = h;
end % mcplot_display




