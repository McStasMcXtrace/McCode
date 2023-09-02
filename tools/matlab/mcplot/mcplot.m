function data = mcplot(varargin)
% mcplot: plot a McCode simulation result
% data = mcplot(filename, ...)
%
% This function displays a McCode simulation result a single window with 
% subplots. It also returns the McStas simulation structures. 
% It may import both McCode and Matlab format data sets.
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
% Version: $Revision$
% Origin: ILL
%
%   This file is part of the McCode neutron/X ray-trace simulation package
%   Copyright (C) 1997-2010, All rights reserved
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

% check for input argument: filename ?
  data = {};
  
  if nargin == 0
    filename = '';
  elseif nargin == 1 && ~isempty(dir(varargin{1}))
    filename = varargin{1};
  else
    save_as = '';
    index_out=0;
    for index= 1 : size(varargin,2)
      this_arg = varargin{index};
      if strncmp(this_arg, '-', 1)  % get option for export 
        save_as=this_arg; 
        switch save_as
        case {'-h','--help'}
          % display help and exit
          disp([ mfilename ': plot a McCode simulation result using the Matlab backend.' ])
          disp('usage:')
          disp([ '    ' mfilename ' FILE|DIR' ])
          disp(  '        Display the specified monitor file or directory.')
          disp([ '    ' mfilename ' [-png|-jpg|-fig|-eps|-pdf] [FILE|DIR]' ])
          disp(  '        Export the specified monitor file or directory to given format.')
          exit
        end
        continue;
      else index_out = index_out+1; end
      this_file = varargin{index};
      if isempty(this_file), return; end
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
    if ~exist('uigetfile')
      % for Octave
      [filename, pathname] = mcplot_uigetfile('*.*', 'Select data file to load');
    else
      [filename, pathname] = uigetfile('*.*', 'Select data file to load');
    end
    if isempty(filename), return; end
    filename = fullfile(pathname, filename);
  end
  
  % import data set
  data = mcplot_load(filename);
  
  % check data structures
  data = mcplot_check_data(data);
  
  % plot (overview or single)
  data = mcplot_display(data);
  
  drawnow;

  % Many versions of Octave needs a pause() here to keep the final plot open.
  if exist ('OCTAVE_VERSION', 'builtin')
    if ~isempty(data)
      if nargin==0 || nargin==1
	display('Pausing --> Please press a key in the octave terminal to continue');
	pause();
      end
    end
  end
end % mcplot (main)

% ==============================================================================
% inline functions
% ==============================================================================

% function match= mcplot_filestrfind(filename, pattern, {buffer})
% function match= mcplot_filefgetl(filename, positions)
% function data = mcplot_load(filename)
% function data = mcplot_load_mccode(filename)
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
  
  invalid         = find(~isstrprop(string,'print'));
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
        this_data = mcplot_load_mccode(filename{index});    % load single McCode monitor files
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

function structure=mcplot_load_mccode(filename)
  % mcplot_load_mccode: load a single data set (not sim) or a structure
  % filename: name of file to load
  %   RETURN: a cell array of data structures
  
  structure = [];
  
  isMcCode = [ mcplot_filestrfind(filename, 'array_1d', 10000) ...
               mcplot_filestrfind(filename, 'array_2d', 10000) ...
               mcplot_filestrfind(filename, 'multiarray_1d', 10000) ];
  
  if isempty(isMcCode), return; end % also returns when filename is empty
  
  disp([ 'Loading ' filename ' (McCode format)' ]);
  % with scilab, a call to fscanfMat will extract data and header
  fid = fopen(filename);
  if fid == -1, return; end % also returns when filename is empty
  
  % read header
  if ~exist('textscan') || exist ('OCTAVE_VERSION', 'builtin')
    [header, data] = mcplot_textscan(fid);
  else
    header = textscan(fid,'#%s','endOfLine','\n','delimiter','\n');
  end
  if iscellstr(header{1}) && length(header)==1
    header = header{1};
  end
  
  paramstr = '';
  % build structure from header fields
  for index=1:length(header)
    % use strtok to split line around':'
    [field, value] = strtok(header{index}, ':');
    value(find(value == ':')) = ''; value=strtrim(value); field=strtrim(field);
    value=strrep(value,'''','');  % remove quotes
    % keep 'value' as a string for further use before converting to num
    value_str=value;  
    num   = str2num(value);
    if ~isempty(num), value = num; end
    if strncmp(field,'Instrument', length('Instrument'))
      field = 'Instrument';
    else
      field = genvarname(field);                        % validate variable name
    end
    if isempty(strfind(field, 'Param'))
      if ~isempty(value), structure = setfield(structure, field, value); end   % set new field
    else  % special case for parameters. Build 'Param' sub structure
      if ~isfield(structure,'Param'), structure = setfield(structure, 'Param',[]); end  % create if needed
      param        = getfield(structure, 'Param');
      % use strtok to split line around'='
      [var,value]  = strtok(value_str,'=');
      value(find(value == '=')) = ''; value=strtrim(value); var=strtrim(var);
      paramstr = [ paramstr var '=' value '; ' ];
      num          = str2num(value);
      if ~isempty(num), value = num; end
      param        = setfield(param, var, value);
      structure    = setfield(structure, 'Param', param);     % store updated sub-structure
    end
  end
  structure = setfield(structure, 'Parameters', paramstr);
  structure.filename = filename;
  clear header
  
  % additional compatibility checks (from McCode/Matlab format)
  if isfield(structure, 'parameters') && ~isfield(structure,'Param')
    param = getfield(structure,'parameters');
    structure = rmfield(structure,'parameters');
    structure = setfield(structure, 'Param', param);
  end
  if ~isfield(structure,'Param')
    structure = setfield(structure, 'Param', 'Unknown instrument parameters');
  end
  
  % extract type and theoretical dimension of data block
  if isfield(structure,'type')
    % use strtok to split line around'()'
    [t s]=strtok(structure.type, '()');
    structure.type = t;
    % remove parenthesis
    s=strrep(s,'(',''); s=strrep(s,')','');
    structure.size = str2num(s);
  end
  
  % load data block
  if exist('textscan') && ~exist ('OCTAVE_VERSION', 'builtin')
    frewind(fid);
    data     = textscan(fid,'%f','CommentStyle','#');
  end % else was obtained in one call to mcplot_textscan
  
  fclose(fid);
  if iscell(data) && length(data)==1
    data = data{1};
  end
  
  structure.data = data;
  clear data
  
  % reshape data block
  if ~isempty(strmatch('array_1d',structure.type))
    if prod(size(structure.data))/prod(structure.size) == 4
      % textscan provides a single vector: we must reshape as 4 columns
      structure.data = transpose(reshape(structure.data,[ 4 structure.size ]));
      % extract signal, errors and events
      if size(structure.data,2) >= 4, structure.events=structure.data(:,4); end
      if size(structure.data,2) >= 3, structure.errors=structure.data(:,3); end
      structure.x     =structure.data(:,1);
      structure.data  =structure.data(:,2);
    end
  elseif ~isempty(strmatch('array_2d',structure.type)) % 2d
    len      = prod(structure.size);
    structure.errors=[];
    structure.events=[];
    if exist('textscan') && ~exist ('OCTAVE_VERSION', 'builtin')  % Matlab read file and produces a single vector to be re-organized
      this_data        = structure.data;
      structure.data   = transpose(reshape(this_data(1:len),structure.size));
      if prod(size(this_data)) >= 2*len
        structure.errors = transpose(reshape(this_data((len+1):(2*len)),structure.size));
      end
      if prod(size(this_data)) >= 3*len
        structure.events = transpose(reshape(this_data((2*len+1):(3*len)),structure.size));
      end
    else
      % we used our own data reader (slower), which already shaped data block
      l = structure.size(1);
      this_data        = structure.data;
      structure.data = this_data(1:l,1:structure.size(2));
      
      if prod(size(structure.data)) >= 2*len
        structure.errors = this_data((l+1):(2*l),1:structure.size(2));
      end
      if prod(size(structure.data)) >= 3*len
        structure.events = this_data((2*l+1):(3*l),1:structure.size(2));
      end
    end
  elseif ~isempty(strmatch('multiarray_1d',structure.type)) % multiarray_1d
    % reshape data block according to the multiarray_1d(dim)
    n = prod(size(structure.data))/prod(structure.size);
    structure.size= [ structure.size n ];
    structure.data = transpose(reshape(structure.data,fliplr(structure.size)));
  end

end % mcplot_load_mccode

% ==============================================================================

function data = mcplot_check_data(structure)
  % mcplot_check_data: check all data sets for consistency
  data={};
  if iscell(structure)
    for index=1:length(structure)
      data = { data{:} mcplot_check_data(structure{index}) };
    end
    return
  end
  
  % check a single structure format
  % fields
  if ~isfield(structure, 'Source')
    if isfield(structure,'Instrument')
      structure.Source = structure.Instrument;
    else
      structure.Source = filename;
    end
  end
  if ~isfield(structure, 'component') structure.component = 'unknown'; end
  if ~isfield(structure, 'filename')  structure.filename  = pwd; end
  if ~isfield(structure, 'errors')    structure.errors    = [];  end
  if ~isfield(structure, 'events')    structure.events    = [];  end
  if ~isfield(structure, 'title'),    structure.title     = '';  end

  % extract type and theoretical dimension of data block
  if ~isfield(structure, 'size')
    structure.size = [];
  end
  if isfield(structure,'type')
    % use strtok to split line around'()'
    [t s]=strtok(structure.type, '()');
    structure.type = t;
    % remove parenthesis
    s=strrep(s,'(',''); s=strrep(s,')','');
    structure.size = str2num(s);
  end
  
  if isempty(structure.size)
    structure.size = size(structure.data);
  end

  % reshape data block from 'type'
  
  data = structure;
  
  % dimensions
end % mcplot_check_data

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
  
  % in principle, McCode/Matlab data format contains its own 'mcload_inline' routine
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
  disp([ 'Loading ' filename ' (Matlab format)' ]);

  [data, parameters] = mcplot_load_structure(data);   % extract monitors recursively

  % insert extracted parameteres in each monitor structure
  if isstruct(data), 
    data.Param=parameters;
  elseif iscell(data)
    for index=1:length(data)
      this_data = data{index};
      if isstruct(this_data), 
        this_data.Param=parameters; 
        data{index} = this_data; 
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
  
  if isfield(s,'data') && isnumeric(s.data)                  % found 'data': we keep that
    data = s;
    data.Param = parameters;
    disp([ 'Loading ' data.filename ]);
    data = { data };
    % return it as a structure
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
            this_data = this_data(1);
          end
          data = { data{:} this_data{:} };
        end
      end
    end
  end
  if iscell(data) && length(data) == 1 && isstruct(data{1}) 
    data=data(1);
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
  disp([ 'Loading ' filename ' (McCode simulation overview)' ]);
  % search for 'filename:' tags, and extract links to other files
  filenameReferences = mcplot_filestrfind(filename, 'filename:');
  filenameLines      = mcplot_filefgetl  (filename, filenameReferences+length('filename:'));
  filepath = fileparts(filename);

  % loop on filenames
  for index = 1:length(filenameLines)
    % calls mcplot_load_mccode
    this_filename = fullfile(filepath, filenameLines{index});
    this_data     = mcplot_load_mccode(this_filename);
    if ~isempty(this_data) && ~isempty(strmatch('multiarray_1d',this_data.type))
      this_data.size = size(this_data.data);
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
  data = mcplot_load_mccode(filename);                

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
  if ~~isempty(strmatch('multiarray_1d',structure.type)), return; end
  
  disp([ 'Loading ' structure.filename ' (extracting McCode scan steps)' ]);
  
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
  xvars        = ~isempty(strmatch(fliplr(strtok(structure.xvars)), column_labels, 'exact'));
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
    this_data.data = intensity;
    this_data.x    = xvars;
    this_data.errors=errors;
    this_data.events=events;
    data = { data{:} this_data };
    disp([ 'Loading ' this_data.filename '#' this_monitor ' (scan steps)' ]);
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
      data{index}.subplot = index;
      data{index} =  mcplot_display(data{index}, fig);
    end
    return
  end
  
  % handle a single plot (a structure)
  if isempty(data), return; end
  
  set(fig, 'Name', [ data.title ':' data.filename ]);
  S = size(data.data);
  if isfield(data,'xylimits')
    l = data.xylimits; 
  else
    l = data.xlimits;
  end
  if ischar(l), l=str2num(l); end
  if ~isempty(strmatch('array_2d',data.type))
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
    h=surface(data.x, data.y, data.data);
    xlim([l(1) l(2)]); ylim([l(3) l(4)]);
    shading flat;
  elseif ~isempty(strmatch('array_1d',data.type))
    if ~isfield(data,'x') || (isfield(data,'x') && length(data.x) ~= size(data.data,1))
      if any(size(data.data) == 1)
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

    % plot the data as a curve with error bars or single curve
    if ~isempty(data.errors)
      h=errorbar(data.x, data.data, data.errors);
    else
      h=plot(data.x, data.data);
    end
    xlim([l(1) l(2)]);
  end
  % add labels, title
  t=title(sprintf('%s\n%s', data.title, data.filename)); 
  set(t,'interpreter','none','fontweight','bold');
  xlabel(data.xlabel);
  ylabel(data.ylabel);
  grid on;
  % store data into plot UserData
  set(h, 'UserData', data, 'Tag',[ 'mcplot_data_' data.filename ]);
  
  % add contextual menu on each plot
  if exist('uicontextmenu') && ~exist ('OCTAVE_VERSION', 'builtin')
    hm = uicontextmenu;
    uimenu(hm, 'Label',['About ' data.filename ],'Callback', ...
      [ 'h=get(gco,''userdata''); t=evalc(''data=h''); p=evalc(''parameters=h.Param'',[]); msgbox([ h.title '': '' h.filename, t, p ], [ h.title '': '' h.filename ],''help'');' ])
      
    uimenu(hm, 'Label', [ 'Source:    '    data.Source ]);
    uimenu(hm, 'Label', [ 'Component: '    data.component ]);
    uimenu(hm, 'Label', [ 'Filename:  '    data.filename ]);
    uimenu(hm, 'Separator','on','Label','Toggle grid', 'Callback','grid');
    if ~isempty(strmatch('array_1d',data.type))
      uimenu(hm, 'Label','Toggle error bars', 'Callback','tmp_h=get(gco,''children''); if strcmp(get(tmp_h(2),''visible''),''off''), tmp_v=''on''; else tmp_v=''off''; end; set(tmp_h(2),''visible'',tmp_v); clear tmp_h tmp_v');
      uimenu(hm, 'Label','Linear/Log scale','Callback', 'if strcmp(get(gca,''yscale''),''linear'')  set(gca,''yscale'',''log''); else set(gca,''yscale'',''linear''); end');
    else
      uimenu(hm, 'Label','Reset Flat/3D View', 'Callback','[tmp_a,tmp_e]=view; if (tmp_a==0 & tmp_e==90) view(3); else view(2); end; clear tmp_a, tmp_e; lighting none;alpha(1);shading flat;rotate3d off;axis tight;');
      uimenu(hm, 'Label','Smooth View','Callback', 'shading interp;');
      uimenu(hm, 'Label','Add Light','Callback', 'light;lighting phong;');
      uimenu(hm, 'Label','Transparency','Callback', 'alpha(0.7);');
      uimenu(hm, 'Label','Linear/Log scale','Callback', 'if strcmp(get(gca,''zscale''),''linear'')  set(gca,''zscale'',''log''); else set(gca,''zscale'',''linear''); end');
    end
    
    % add rotate/pan/zoom tools in case java machine is not started
    if ~usejava('jvm')
      uimenu(hm, 'Separator','on','Label','Zoom on/off', 'Callback','zoom');
      uimenu(hm, 'Label','Pan (move)', 'Callback','pan');
      if ~isempty(strmatch('array_2d',data.type))
        uimenu(hm, 'Label', 'Rotate', 'Callback','rotate3d on');
      end
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
    
    set(h, 'UIContextMenu', hm);
  end
  
  % add figure menu
  reset_callback = 'lighting none;alpha(1);shading flat;axis tight;rotate3d off';
  if ~isfield(data,'subplot'), filename = data.filename;
  else
    pathname = fileparts(data.filename);
    filename = fullfile(pathname, 'mccode');
  end
  if exist('uimenu') && (~isfield(data,'subplot') || data.subplot==1) && ~exist ('OCTAVE_VERSION', 'builtin')
    % in case of nojvm, we create a small button to attach the menu on
    if ~usejava('jvm')
      button = uicontrol(gcf, 'style','pushbutton', 'position', [ 5 5 20 20 ], 'String','>',...
      'BackgroundColor','green','Tooltip','Reset view. Right click for menu.',...
      'Callback',reset_callback);
      hm = uicontextmenu;
    else
      hm = uimenu(gcf, 'Label','McCode','Accelerator','m', 'Tag', 'McCode_menu');
    end
    uimenu(hm, 'Label',['Save as ' filename '.png'], 'Callback', ...
      [ 'print(''-dpng'',''-noui'', ''' filename '.png''); disp(''Exported as ' filename '.png'')' ]);
    uimenu(hm, 'Label',['Save as ' filename '.jpg'], 'Callback', [ 'print(''-djpeg'',''-noui'', ''' filename '.jpg''); disp(''Exported as ' filename '.jpg'')' ]);
    uimenu(hm, 'Label',['Save as ' filename '.fig'], 'Callback', [ 'saveas(gcf, ''' filename '.fig''); disp(''Exported as ' filename '.fig'')' ]);
    uimenu(hm, 'Label',['Save as ' filename '.eps'], 'Callback', [ 'print(''-depsc'',''-noui'', ''' filename '.eps''); disp(''Exported as ' filename '.eps'')' ]);
    uimenu(hm, 'Label',['Save as ' filename '.pdf'], 'Callback', [ 'print(''-dpdf'',''-noui'', ''' filename '.pdf''); disp(''Exported as ' filename '.pdf'')' ]);
    
    uimenu(hm, 'Separator','on', 'Label', 'About McCode/McPlot', ...
      'Callback','msgbox({''MCPLOT a Tool to display McCode data set'',''E.Farhi, ILL 2010 <www.mccode.org>''},''About McCode/mcplot'',''help'')');
    % add rotate/pan/zoom tools in case java machine is not started
    if ~usejava('jvm')
      uimenu(hm, 'Label','Print ...', 'Callback', 'printdlg(gcf)');

      uimenu(hm, 'Separator','on','Label','Zoom on/off', 'Callback','zoom');
      uimenu(hm, 'Label','Pan (move)', 'Callback','pan');
      if ~isempty(strmatch('array_2d',data.type))
        uimenu(hm, 'Label', 'Rotate', 'Callback','rotate3d on');
      end
      uimenu(hm, 'Label','Reset view/menu', 'Callback',reset_callback);
      set(button, 'UIContextMenu', hm);
    end
  end
  if isfield(data,'subplot'), data=rmfield(data,'subplot'); end
  % store data into plot UserData
  set(h, 'UserData', data, 'Tag',[ 'mcplot_data_' data.filename ]);
  
  % add an entry in the 'McCode' figure menu
  hm = findobj(gcf, 'Tag', 'McCode_menu');
  if isempty(hm) 
    hm = uimenu(gcf, 'Label','McCode','Accelerator','m', 'Tag', 'McCode_menu');
  end
  uimenu(hm, 'Label',[ 'Open ' data.component ], 'Callback', ...
      [ 'h=findobj(gcf, ''Tag'',[ ''mcplot_data_' data.filename ''' ]);' ...
        'h=get(h,''userdata''); h.filename = ''' data.component '''; mcplot(h);' ]);

  % update data structure
  data.handle = h;

end % mcplot_display

% ==============================================================================
% Matlab -> Octave compatibility layer
% ==============================================================================

%% uigetfile: a file selector
%% A list of files is displayed, and a prompt requests user's choice.
%% Whenever Xdialog, kdialog (KDE) or zenity (Gnome) are present, a nice file 
%% selector will be opened.
%%
%% Usage:
%%   [filename, pathname, filterindex ] = uigetfile(filter, title);
%% If dialog is canceled, then zeros are returned.
%%
%% Example:
%%   [fname, pname ] = uigetfile('*.m', 'Select an m-file');
%%
%% Author: Petr Mikulik, Emmanuel Farhi
%% Version: October 2009
%% License: Free as much as possible / public domain.

function [a, b, c] = mcplot_uigetfile (files, title)

  if nargin==0 files='*'; end
  if nargin<2  title='Choose file'; end

  a=0; b=[];

  c=1; % no support for file filters more than 1
  
  % check that selection is non void
  d = dir(files);
  if isempty(d)
    disp('No (such) file available.');
    a=0; b=0; c=0;
    return
  end
  
  if isempty(b) % try with Xdialog
    [a,b]=system(['Xdialog --stdout --title "', title, '" --fselect ''', files, ''' 0 0 2>/dev/null' ]);
    if a==1 && isempty(b), return; end % User pressed Cancel
  end

  if isempty(b) % try with Kdialog (takes time to init/close on non KDE systems)
    [a,b]=system(['kdialog --getopenfilename . ''', files, ''' --title "', title, '" 2>/dev/null' ]);
    if a==1 && isempty(b), return; end % User pressed Cancel
  end
  
  if isempty(b) % try with Zenity (filter selection does not work properly)
    [a,b]=system([ 'zenity --file-selection --filename=''', files, ''' --title="', title, '" 2>/dev/null' ]);
    if a==1 && isempty(b), return; end % User pressed Cancel
  end

  if isempty(b) % fall back solution using 'dir'
    fprintf('\n=== [%s: %s] ===\n', title, files);
    fprintf('       Size         Date           File Name\n');
    list = '';
    for index=1:length(d)
      if ~d(index).isdir
        if d(index).bytes > 1073741824
          fprintf('[%7f Gb][%s] %s\n', d(index).bytes/1073741824, d(index).date, d(index).name);
        else
          fprintf('[%10i][%s] %s\n', d(index).bytes, d(index).date, d(index).name);
        end
      else
        fprintf('[%s][%s] %s%c\n', ' Directory', d(index).date, d(index).name,filesep);
      end
    end
    b=input('Please enter filename: (RETURN to Cancel)', 's');
    if ~exist(b, 'file') b={}; end
    a='';
    if length(b)>0 && b(1)~=filesep b=[pwd, filesep, b]; end
  end

  if ~isempty(b)
    if b(end)=='\n' b=b(1:end-1); end
  end

  % no file selected => a and b are zeros
  if isempty(b)
      a=0; b=0; c=0;
      return
  end

  % split b into name and path
  k=rindex(b, filesep());
  a=b(k+1:end);
  b=b(1:k);

end % mcplot_uigetfile

% ==============================================================================

function [header, data] = mcplot_textscan(file)
  % mcplot_textscan: extract header and data blocks from McCode data format
  % reads a McCode data file, putting all comment lines '#...' into header, 
  % and the rest in data
  
  if ischar(file), fid = fopen(file);
  else             fid = file; end
  
  header={};
  data  ={};
  current_data = [];
  flag_exit = 0;
  
  while flag_exit == 0
    line = fgetl(fid);
    if line == -1, flag_exit=1; break; end  % reached EOF: end reading
    [first_tok, reminder] = strtok(line);
    switch first_tok
      case {'#','%','//'} % detected comment line
        header = { header{:} reminder };
      otherwise           % consider this is data
        % replace non numerics into spaces, and keep [0-9] +- eE
        index = find(~isdigit(line) & line ~= '+' & line ~= '-' & line ~= 'e' & line ~= 'E');
        line = sscanf(line, '%g');  % get numerical values
        line = transpose(line(:));  % a row
        % check if we are currently catenating into a block
        if ~isempty(current_data)
          % same number of columns ? catenate to current data block
          if size(current_data, 2) == size(line, 2)
            current_data = [ current_data ; line ];
          else
           % number of columns changed ? create new data block
           data = { data{:} current_data };
           current_data=[];
          end
        end
        if isempty(current_data), current_data=line; end
      end
  end % while
  
  if ~isempty(current_data)
    data = { data{:} current_data };
  end
  
  if ischar(file), fclose(fid); end

end % mcplot_textscan

