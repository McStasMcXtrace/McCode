function data = mcplot(varargin)
// mcplot: plot a McCode simulation result
// data = mcplot(filename, ...)
// 
// This function displays a McCode simulation result a single window with 
// subplots. It also returns the McStas simulation structures. 
// 
// input:
//  filename: one or more simulation name(s) or directory
//          or a single detector file name
//          if filename does not exist, a file selector is called.
//  optional string: -bmp, -eps, -emf, -gif, -ps, -ppm 
//            will export further figures directly to files
// 
// output:
//  data: a cell of detector structures
// 
// examples:
//   exec('mcplot.sci',-1) 
//   mcplot
//   mcplot mcstas.sim
//   mcplot -png simulation
// 
// Written by: E. Farhi
// Date: April 16st 2010
// Release: McStas 1.6
// Version: $Revision: 1.229 $
// Origin: ILL
// 
//   This file is part of the McCode neutron/X ray-trace simulation package
//   Copyright (C) 1997-2010, All rights reserved
//   Risoe National Laborartory, Roskilde, Denmark
//   Institut Laue Langevin, Grenoble, France
// 
//   This program is free software; you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation; version 2 of the License.
// 
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
// 
//   You should have received a copy of the GNU General Public License
//   along with this program; if not, write to the Free Software
//   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 

// check for input argument: filename ?
  data = list();
  
  if argn(2) == 0 // nargin
    filename = '';
  elseif argn(2) == 1
    filename = varargin(1);
  else
    save_as = '';
    index_out=0;
    for index= 1 : size(varargin,2)
      this_arg = varargin(index);
      if this_arg(1)=='-'  // get option for export 
        save_as=this_arg; 
        continue;
      else index_out = index_out+1; end
      this_file = varargin(index);
      this_data = mcplot(this_file);
      data($+1) = this_data;
      
      // check for direct export options
      if length(save_as)
        win=gcf();
        if ishandle(win), win=win.figure_id; end
        select save_as
        case '-bmp'
          xs2bmp(win, [ this_file '.bmp' ]); disp(['Exported as ' this_file '.bmp']);
        case '-emf'
          xs2emf(win, [ this_file '.emf' ]); disp(['Exported as ' this_file '.emf']);
        case '-gif'
          xs2gif(win, [ this_file '.gif' ]); disp(['Exported as ' this_file '.gif']);
        case '-eps'
          xs2eps(win, [ this_file '.eps' ]); disp(['Exported as ' this_file '.eps']);
        case '-ps'
          xs2ps(win, [ this_file '.ps' ]); disp(['Exported as ' this_file '.ps']);
        case '-ppm'
          xs2ppm(win, [ this_file '.ppm' ]); disp(['Exported as ' this_file '.ppm']);
        else
          save_as = '';
        end
        if length(save_as), close(win); end
      end
    end
    if length(data) == 1, data=data(1); end
    return
  end

  if ~length(filename)
    if mtlb_exist('uigetfile')
      [filename,pathname] = uigetfile('*.*', pwd(), 'Select data file to load');  // uigetfile
      filename = fullfile(pathname, filename);
    else
      filename = xgetfile('*.*', pwd(), 'Select data file to load');  // uigetfile
    end
    if ~length(filename), return; end
  end
  
  // import data set
  data = mcplot_load(filename);
  
  // plot (overview or single)
  data = mcplot_display(data);
  
  drawnow;  // we have set immediate_drawing='off' in mcplot_display

endfunction // // mcplot (main)

// ==============================================================================
// inline functions
// ==============================================================================

// function match= mcplot_filestrfind(filename, pattern, (buffer))
// function match= mcplot_filefgetl(filename, positions)
// function data = mcplot_load(filename)
// function data = mcplot_load_mccode(filename)
// function data = mcplot_load_scilab(filename)
// function data = mcplot_load_structure(s)
// function data = mcplot_load_sim(filename)
// function data = mcplot_load_scan(filename)
// function data = mcplot_split_multiarray(structure)
// function data = mcplot_display(structs) 

// ==============================================================================

function match = mcplot_filestrfind(filename, str, buffer)
  // mcplot_filestrfind: find matches of 'str' in the specified filename
  // filename: name of file to search into
  //   str:    pattern to search for
  //   buffer: size of buffer used to parse file. buffer=0 will read the entire file
  //           a finite buffer size will only read that amount within the file,
  //           corresponding e.g. with the header.
  //   RETURN: positions of matches within the file
  
  match = [];
  
  if argn(2) < 2, return; end
  if argn(2) < 3, 
    buffer = -1; // will read whole file iteratively
  end
  if buffer == -1,  buffer   = 0;      readToEOF= 1; else readToEOF = 0; end
  if ~buffer,       buffer   = 10000;  end  // default buffer size
  invalid      = find(~isprint(str));
  str          = ascii(str);
  str(invalid) = 32;  // replace non printable characters with spaces
  str          = ascii(str);
  
  if iscellstr(filename), filename = filename(1); end
  
  [fid, err] = mopen(filename,'r'); // fopen
  if err < 0, return; end   // also returns when filename is empty
  
  offset = 0; // offset at which the buffer must be loaded from file
  
  // read file by blocks, once or iteratively
  while (~meof(fid)) // feof
    block = mgetstr(buffer, fid);   // fread
    if length(block)
      invalid        = find(~isprint(block));
      block          = ascii(block);
      block(invalid) = 32; // replace non printable characters with spaces
      block          = ascii(block);
      
      // find search string in the block
      this_match = strindex(block, str);          // strfind: search string
      if length(this_match)
        match = [ match (this_match+offset-1) ];  // append new match locations (absolute)
      end
      
      // prepare new offset for iterative search
      offset = mtell(fid);  // ftell
      if offset == -1, readToEOF = 0; end
    else 
      readToEOF = 0; 
    end
    if readToEOF == 0, break; end
  end // while (readToEOF)
  
  mclose(fid);  // fclose: close file
  
endfunction // // mcplot_filestrfind

// ==============================================================================

function match = mcplot_filefgetl(filename, positions)
  // mcplot_filefgetl: extract iteratively lines at given positions in file
  //  filename: name of file to search into
  // positions: positions of matches within the file
  // RETURN: cellstr of lines corresponding to positions in file
  
  match = list();
  
  [fid, err] = mopen(filename,'r'); // fopen

  if err < 0, return; end // also returns when filename is empty
  
  for index=1:length(positions)
    mseek(positions(index), fid, 'set');  // fseek: jump at match position
    line = mgetl(fid,1);                  // fgetl
    if ~ischar(line), break; end
    match($+1) = stripblanks(line);       // strtrim
  end
  
  mclose(fid);  // fclose: close file
endfunction // // mcplot_filefgetl

// ==============================================================================

function data=mcplot_load(filename)
  // mcplot_load: loads data from directory or file name
  //   Handles McCode and Matlab format, as overview (.sim), directory and single data file
  // filename: name of file to load, or a structure
  //   RETURN: a single structure or a cell array of data structures
  
  data = list();
  if ishandle(filename)
    data = get(filename, 'user_data');
  elseif isstruct(filename)
    [data,parameters] = mcplot_load_structure(filename);
  elseif iscellstr(filename)
    for index=1:length(filename)
      data($+1) = mcplot_load(filename(index)) ;
    end
    return
  elseif isdir(filename)  // search for mcstas.sim, mcstas.dat in director
    test_files=list('mcstas.sim','mcstas.dat','mccode.sim','mccode.dat','mcstas.sci','mccode.sci');
    for index=1:length(test_files)
      this_dir = dir(fullfile(filename, test_files(index)));
      if size(this_dir.name,1), data = mcplot_load(this_dir.name); break; end
    end
    // else get all files in directory
  end
  if ~length(data)
    this_dir = dir(filename);           // get file reference (and test for existence)
    if ~length(this_dir), return; end   // invalid. exit. Also returns when filename is empty
    
    // find directory entries that are not directories, and non NULL
    index    = find(~this_dir.isdir);
    this_dir = this_dir.name;
    if isdir(filename), this_dir=filename+filesep()+this_dir; end
    filename = (this_dir(index,:));        
    // filename is now a cellstr: IMPORT HERE
    for index=1:size(filename,2)
      // test if this is a sim (contains 'begin instrument') or multiarray_1d file
      // in this case, the returned value is a full data set
      this_data = mcplot_load_sim(filename(index));       // load McCode SIM (overview) files
      if ~length(this_data)
        this_data = mcplot_load_scan(filename(index));    // load McCode scan files (multiarray_1d)
      end
      if ~length(this_data)
        this_data = mcplot_load_scilab(filename(index));  // load Scilab format files
      end
      if ~length(this_data)
        this_data = mcplot_load_mccode(filename(index));    // load single McCode monitor files
      end
      data($+1) = this_data;
    end
  end
  // in case result is embedded inside a cell, we extract it
  if iscell(data) & length(data) == 1
    data = data(1);
  end

endfunction // // mcplot_load

// ==============================================================================

function structure=mcplot_load_mccode(filename)
  // mcplot_load_mccode: load a single data set (not sim nor multiarray) or a structure
  // filename: name of file to load
  //   RETURN: a cell array of data structures
  
  structure = struct();
  data = [];
  header=[];
  
  if ischar(filename)
    disp('Loading '+filename+' (McCode format)');
    // with scilab, a call to fscanfMat will extract data and header
    [data, header] = fscanfMat(filename); // a nice textread equivalent
    
    
    paramstr = '';
    // build structure from header fields
    for index=1:size(header,1)
      line = strtok(header(index),':');                             // strtok extract field:value
      if length(line) < 2, continue; end
      value=stripblanks(strsubst(strcat(line(2:$)),'''',''));       // strtrim/strrep: remove blanks
      [num,err] = evstr(value);
      if length(num), value = num; end
      field=strsubst(line(1) ,'#',''); field=stripblanks(strsubst(field ,':','')); // remove comment and : sign
      if strcmp(part(field,1:10),'Instrument')
        field = 'Instrument';
      else
        field = genvarname(field);                                  // validate variable name
      end
      if ~length(strindex(field, 'Param'))                          // strfind
        if length(value), structure(field)=value; end   // set new field
      else  // special case for parameters. Build 'Param' sub structure
        if ~isfield(structure,'Param'), structure.Param=[]; end     // create if needed
        param     = structure.Param;
        paramstr  = paramstr+value+'; ';
        var       = strtok(value,'=');                                    // get 'parameter=value' pair
        value     = var(2); var=genvarname(part(var(1),1:length(var(1))-1));
        [num,err] = evstr(value);
        if length(num), value = num; end
        param(var)= value;
        structure.Param = param;                                    // store updated sub-structure
      end
    end
    structure.Parameters = paramstr;
    structure.filename   = filename;
    structure.data = data;
  elseif isstruct(filename)
    data=[];
    structure=filename;
  end
  
  // extract type and theoretical dimension of data block
  if isfield(structure,'type')
    var = strtok(structure.type, '(' );     // strtok
    t = part(var(1),1:(length(var(1))-1));  // extract type
    s = evstr(strsubst(var(2), ')',''));    // and size
    structure.type = t;
    structure.size = s;
  else
    return;
  end
  
  // additional compatibility checks (from McCode/Matlab format)
  if isfield(structure, 'parameters') & ~isfield(structure,'Param')
    structure.Param = structure.parameters;
  end
  if ~isfield(structure,'Param')
    structure.Param = 'Unknown instrument parameters';
  end
  
  if isfield(structure, 'data')
    data = structure.data;
  end
  
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

  if ~length(data), return; end

  // handle multiarray dimension (number of scan steps)
  if length(structure.size) == 1
    n = length(data)/structure.size;
    m = structure.size;
    structure.size = [m n];
  end
  
  // check if data is transposed
  if size(data')==structure.size
    structure.size=flipdim(structure.size,2); // fliplr
  end
  
  if ischar(filename)
    // reshape data block from 'type'
    len      = prod(structure.size);
    signal   = data(1:structure.size(1), 1:structure.size(2));
    if prod(size(data)) >= 2*len
      errors = data((1:structure.size(1))+structure.size(1), 1:structure.size(2));
    else 
      errors = []; 
    end
    if prod(size(data)) >= 3*len
      events = data((1:structure.size(1))+2*structure.size(1), 1:structure.size(2));
    else 
      events = []; 
    end
    // add data block
    structure.data     = signal;
    structure.errors   = errors;
    structure.events   = events;
  end

endfunction // // mcplot_load_mccode

// ==============================================================================

function data=mcplot_load_scilab(filename)
  // mcplot_load_matlab: test and open a McCode/Matlab data file and load its content
  // filename: name of file to load (matlab format)
  //   RETURN: a data structure, or empty if not a McCode/Matlab file
  
  data = list();
  // search for 'Scilab' in header. Return if not found
  isScilabScript = [ mcplot_filestrfind(filename, 'Scilab', 10000) ...
                     mcplot_filestrfind(filename, 'scilab', 10000) ...
                     mcplot_filestrfind(filename, '// Embedded function for building', 10000) ];

  if ~length(isScilabScript), return; end // also returns when filename is empty
  
  // in principle, McCode/Scilab data format contains its own 'mcload_inline' routine
  // make sure we get a Scilab file
  [path,fname,extension]=fileparts(filename);
  p=pwd();
  cd(path);
  w = warning('query');
  warning('off');
  exec(fname+extension, -1);
  cd(p)
  data = mc_mcstas;
  data.filename = filename;
  disp('Loading '+filename+' (Scilab format)');
  
  [data, parameters] = mcplot_load_structure(data);   // extract monitors recursively
  warning(w);
  // insert extracted parameteres in each monitor structure
  if isstruct(data), 
    data.Param=parameters;
  elseif iscell(data)
    for index=1:length(data)
      this_data = data(index);
      if isstruct(this_data), 
        this_data.Param=parameters; data(index) = this_data; 
      end
    end
  end
  
endfunction // // mcplot_load_scilab

// ==============================================================================

function [data,parameters]=mcplot_load_structure(s,parameters)
  // mcplot_load_structure: load data from a structure recursively
  //         s: structure with possibly full hierachy. We then search for those that contain some 'data'
  //    RETURN: a single structure or a cell array of data structures
  
  if argn(2)<2,
    parameters=[];
  end
  data = list();
  
  if ~isstruct(s), return; end
  
  if isfield(s,'data') & isnumeric(s.data)                  // found 'data': we keep that
    data = s;
    data = mcplot_load_mccode(data);      // check structure
    data.Param = parameters;
  elseif isfield(s,'name') & strcmp(s.name, 'parameters')
    parameters = s;
  else
    tag_names = getfield(1,s); tag_names=tag_names(3:$);    // fieldnames
    for index=1:size(tag_names,2)       // scan recursively structure fields
      d = s(tag_names(index));
      if isstruct(d)
        [this_data,parameters] = mcplot_load_structure(d,parameters);
        if length(this_data)
          if iscell(this_data) & length(this_data) == 1 & isstruct(this_data(1))
            this_data = this_data(1);
          end
          data($+1) = this_data;
        end
      end
    end
  end
endfunction // // mcplot_load_structure
  
// ==============================================================================

function data=mcplot_load_sim(filename)
  // mcplot_load_sim: test and open a .sim (overview) McCode data file and load its content
  // filename: name of file to load
  //   RETURN: a cell array of data structures, or empty if not a scan file
  
  data = list();
  
  // search for 'begin instrument' in header. Return if not found
  isSimFile    = mcplot_filestrfind(filename, 'begin instrument', 10000);
  if ~length(isSimFile)
    return  // also returns when filename is empty
  end
  
  // search for 'filename:' tags, and extract links to other files
  filenameReferences = mcplot_filestrfind(filename, 'filename:');
  filenameLines      = mcplot_filefgetl  (filename, filenameReferences+length('filename:'));
  filepath = fileparts(filename);

  // loop on filenames
  for index = 1:length(filenameLines)
    // calls mcplot_load_mccode
    this_filename = fullfile(filepath, filenameLines(index));
    this_data     = mcplot_load_mccode(this_filename);
    if strcmp(this_data.type,'multiarray_1d')
      this_data = mcplot_split_multiarray(this_data);
    end
    data($+1) = this_data; // IMPORT sim file references
  end

endfunction // // mcplot_load_sim

// ==============================================================================

function data=mcplot_load_scan(filename)
  // mcplot_load_scan: test and open a multiarray (scan) McCode data file and load its content
  // filename: name of file to load
  //   RETURN: a cell array of data structures, or empty if not a sim/scan file
  
  data = list();
  
  // search for 'multiarray_1d' in header. Return if not found
  isMultiArray = mcplot_filestrfind(filename, 'multiarray_1d',    10000);
  if ~length(isMultiArray)  
    return  // also returns when filename is empty
  end
  
  // // IMPORT multiarray file
  data = mcplot_load_mccode(filename);                      

  // then extract columns in a loop
  data = mcplot_split_multiarray(data);
  
endfunction // // mcplot_load_scan

// ==============================================================================

function data = mcplot_split_multiarray(structure)
  // mcplot_split_multiarray: load a multiarray data set and generate monitor column structures
  // structure: single data set structure (multiarray type)
  //    RETURN: a cell array of data structures, one for each monitor in the multiarray
  
  data = list();
  
  // first check if this is a multiarray
  if ~strcmp(structure.type, 'multiarray_1d'), return; end
  
  // remove duplicated spaces
  tmp = strsubst(structure.variables,'  ',' ');
  while length(tmp) ~= length(structure.variables)
    structure.variables = tmp;
    tmp = strsubst(structure.variables,'  ',' ');
  end
  // get column labels, and sort them into I ERR and scan variables
  column_labels=strtok(structure.variables,' ')
  monitor_I   = [];
  monitor_ERR = [];
  scan_labels = [];
  xvars       = [];
  for index=1:size(column_labels,1)
    // we reverse strings so that _I and _ERR are starting labels
    tmp=stripblanks(ascii(flipdim(ascii(column_labels(index)),2)));
    if     strcmp(part(tmp, 1:2), 'I_'),   monitor_I =   [ monitor_I index ];
    elseif strcmp(part(tmp, 1:4), 'RRE_'), monitor_ERR = [ monitor_ERR index ];
    // get indices that refer to scan variable parameters (i.e. not monitors)
    else   scan_labels = [ scan_labels index ];
    end
    if strindex(column_labels(index), strtok(structure.xvars,' ')), xvars=index; end
  end

  // check consistency
  if (length(monitor_I) ~= length(monitor_ERR))
    warning(sprintf('File: %s: Found %d Monitor intensity, and %d corresponding error entries', ...
      structure.filename, length(monitor_I), length(monitor_ERR)) );
  end
  
  if ~length(xvars)
    warning(sprintf('File: %s: Can not find scanned variable ''%s'' within columns\n%s\n', ...
      structure.filename, structure.xvars, structure.variables) );
    xvars = linspace(structure.xlimits(1), structure.xlimits(2), structure.size(1))';
  else
    xvars          = structure.data(:,xvars);
  end
  // gerenate monitor entries from the scanned data
  for index=1:length(monitor_I)
    this_monitor = column_labels(monitor_I(index));
    this_data = structure;   // initiate single monitor data set from the scan structure
    this_data.type = 'array_1d';
    this_data.size = [ structure.size(1) 4 ];
    this_data.title= structure.title+':'+this_monitor;
    this_data.component=this_monitor;
    
    intensity      = structure.data(:,monitor_I(index));
    errors         = structure.data(:,monitor_ERR(index));
    events         = 0*errors+1;
    this_data.data = [ xvars intensity errors events ];
    data($+1) = this_data;
  end
endfunction // // mcplot_split_multiarray

// ==============================================================================

function data = mcplot_display(data, f) 
  // opens a new window, with subplots and plots each structure in axes
  // data: single monitor structure or a cell array of monitor structures
  //  f: optional figure handle to use, else a new figure is created.
  //  RETURN: updated data sets (with plot handle)
  
  h=[];
  if argn(2) < 2, f = []; end
  if f == 0,   f = []; end
  if ~ishandle(f), f = scf(); end  // create a new figure if required
  
  if iscell(data)
    m = floor(sqrt(length(data)));
    n = ceil(length(data)/m);
    for index=1:length(data)
      subplot(m,n, index);
      data(index) =  mcplot_display(data(index), f);
    end
    return
  end
  
  // handle a single plot (a structure)
  if ~length(data), return; end
  if ~isfield(data,'title'), return; end
  f.immediate_drawing='off';  // much faster to hold drwaing until the end
  f.figure_name= data.title+':'+data.filename+' (%d)';
  S = data.size;
  if isfield(data,'xylimits')
    l = data.xylimits; 
  else
    l = data.xlimits;
  end
  if ischar(l), [nl,err]=evstr(l); if length(nl), l=nl; end end
  if strcmp(data.type,'array_2d')
    // builds the XY axes from limits
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
    // plot the data as a surface
    e=surf(data.x, data.y, data.data, 'FaceColor','interp');
    e=gce();
    e.color_mode=-1;
    jet();
    a=gca(); 
    a.data_bounds(1:2,1:2) = [l(1) l(3);l(2) l(4) ];
  elseif strcmp(data.type,'array_1d')
    if ~isfield(data,'x') | (isfield(data,'x') & length(data.x) ~= size(data.data,1))
      if or(size(data.data) == 1)
        // builds the X axis from limits
        if length(strindex(data.title, 'Scan of'))
          // check if this is a scan
          data.x=linspace(l(1),l(2),prod(S));
        else
          //  or normal bin-centered monitor
          if max(S) > 1, data.stepx=abs(l(1)-l(2))/(max(S)-1);
          else           data.stepx=0; end
          data.x=linspace(l(1)+data.stepx/2,l(2)-data.stepx/2,prod(S));
        end
      else
        data.x     =data.data(:,1);
      end
    end
    if (~isfield(data,'errors') | size(data.errors,2) == 0) & and(size(data.data) > 1)
      if size(data.data,2) >= 4, data.events=data.data(:,4); end
      if size(data.data,2) >= 3, data.errors=data.data(:,3); end
      data.data  =data.data(:,2);
    end
    // plot the data as a curve with error bars or single curve
    if isfield(data, 'errors') & length(data.errors)
      errorbar(data.x, data.data, data.errors);
    else
      plot(data.x, data.data);
    end
    xa=gca(); 
    a.data_bounds(1:2,1) = [l(1) ; l(2) ];
  end
  // add labels, title
  a=gca(); 
  a.title.text  =sprintf('%s: %s', data.title,data.filename);
  a.x_label.text=data.xlabel;
  a.y_label.text=data.ylabel;
  a.user_data = data;

  // update data structure
  data.handle = a;
endfunction // // mcplot_display

// ==============================================================================
// Matlab -> Scilab compatibility
// ischar
// iscell
// ishandle
// strcmp
// genvarname
// jet
// errorbar
// isalnum
// isletter
// isdigit
// islower
// isupper
// isprint
// ==============================================================================

function yesno=iscell(object)

  yesno = typeof(object);
  if yesno == "list", yesno=1; else yesno=0; end

endfunction //

function yesno=ishandle(object)

  yesno = typeof(object);
  if yesno == "handle", yesno=1; else yesno=0; end

endfunction //

function yesno=ischar(object)

  yesno = typeof(object);
  if yesno == "string", yesno=1; else yesno=0; end

endfunction //

function yesno=isnumeric(object)

  yesno = typeof(object);
  if yesno == "constant", yesno=1; else yesno=0; end

endfunction //

function name = genvarname(name)

  name = stripblanks(name);
  invalid = find(~isalnum(name));
  name = ascii(name);
  name(invalid)=ascii('_');
  if length(name)>24, name=name(1:24); end
  name = ascii(name);

endfunction //

function j=jet()
// jet colormap (approximation)
  c=[linspace(0,1,15), ones(1,15), linspace(1,0,15)];
  r=[(1:24)*0,c(1:($-5))];
  g=[(1:8)*0,c,(1:11)*0];
  b=[c(6:$),(1:24)*0];
  j=[r' g' b'];
  set(gcf(),'color_map',j);
endfunction //

function errorbar(x,y,e)
// function for creating simple errorbar plots...
  // first, estimate plot range
  xmin=min(x);
  xmax=max(x);
  ymin=min(y-e);
  ymax=max(y+e);
  plot(x,y);
  errbar(x,y,e,e);
endfunction // // errorbar

function ret=strcmp(a,b)

  ret=(a == b);

endfunction //

function str=strtok(value, delimiter)

  str = strindex(value, delimiter);
  if length(str), str=strsplit(value,str); else str=value; end

endfunction //

// ==============================================================================

function ret = isdigit(c)
  c = ascii(c);
  ret = ( c >= ascii('0') & c <= ascii('9') );
endfunction //

function ret = islower(c)
  c = ascii(c);
  ret = ( c >= ascii('a') & c <= ascii('z') );
endfunction //

function ret = isupper(c)
  c = ascii(c);
  ret = ( c >= ascii('A') & c <= ascii('Z') );
endfunction //

function ret = isletter(c)
  ret = ( islower(c) | isupper(c) );
endfunction //

function ret = isalnum(c)
  ret = ( isdigit(c) | isletter(c) );
endfunction //

function ret = isprint(c)
  ret = ( ispunct(c) | isalnum(c) );
endfunction //

function ret = ispunct(c)
  ret = ( isgraph(c) & ~isalnum(c) );
endfunction //

function ret = isgraph(c)
  c = ascii(c);
  ret = ( c >= ascii('!') & c <= ascii('~') );
endfunction //

// ==============================================================================

