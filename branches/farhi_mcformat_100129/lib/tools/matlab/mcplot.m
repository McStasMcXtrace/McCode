function [object,count]=mcplot(object, options, id)
% mcplot: plot a McStas simulation result
% [object,count]=mcplot(object, options, id)
%
% This function displays a McStas simulation result either as many windows
% or on a single window with subplots. It also returns the McStas simulation
% structure. An 'id' may be specified for filtering names within structure.
%
% input:
%  object: one or more simulation name(s) or a single mcstas structure
%          or a single detector file name
%          if filename does not exist, a file selector is called.
%  options may contain keywords
%    '-overview' to plot all results on the same window
%    '-plot'   to plot all results in separate windows
%    '-png'    to export as a PNG file
%    '-ps'     to export as a PostScript file
%    '-psc'    to export as a color PostScript file
%    '-eps'    to export as a EPS file
%    '-epsc'   to export as a color EPS file
%    '-jpg'    to export as an JPEG file
%    '-tif'    to export as a TIFF file
%    '-fig'    to export as a Matlab figure (open)
% id is a keyword used for searching within McStas structure fields
%    'filename' and 'title'
%
% examples:
%   mcplot;
%   s=mcplot('ask me the file','-overview')
%   mcplot(s, 'plot -png', 'MyMonitorName');
%
% Written by: E. Farhi
% Date: 21st, March 2003
% Release: McStas 1.6
% Origin: ILL
%
%   This file is part of the McStas neutron ray-trace simulation package
%   Copyright (C) 1997-2004, All rights reserved
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

% parameter check
if nargin == 0, object=''; end
if nargin <= 1, options=''; end
if nargin <= 2, id = ''; end

if ~ischar(options),  options = ''; end
if ~length(options), options = '-overview'; end
options = lower(options);  % to lower case
if ~length(findstr(options,'plot')) &  ~length(findstr(options,'overview')) &  ~length(findstr(options,'action'))
  options = [ options '-overview ' ];
end
if strcmp(options,'action')
  mcplot_menu_action(object, id);
  return
end

pathname = '';
filename = '';

% handle file name specification in 'object'
if ischar(object) % if object is a string
  if size(object,1) > 1
    for index=1:size(object,2)
      [object,count] = mcplot(object(index), options, id);
    end
    return
  end
  % if object is a '' string, s = 'mcstas'.
  if ~length(object), object = 'mcstas.m'; end
  % checks for directories
  if exist(object,'dir'), object = [ object filesep 'mcstas.m']; end
  [fid, err] = fopen(object, 'r');
  if fid == -1 % error occured. Calls fileselector (uigetfile)
    if length(findstr(options,'-ps')) ...
    | length(findstr(options,'-psc')) ...
    | length(findstr(options,'-eps')) ...
    | length(findstr(options,'-epsc')) ...
    | length(findstr(options,'-jpg')) ...
    | length(findstr(options,'-tif')) ...
    | length(findstr(options,'-png')) ...
    | length(findstr(options,'-fig'))
      fprintf(2,'mcplot: Could not open file %s for auto export\n',object);
      return;
    else
      [object, pathname] = uigetfile('*.m', 'Select a McStas/Matlab simulation file to load');
    end
    if ~ischar(object), return; end
    object = [ pathname filesep object];
    [fid, err] = fopen(object, 'r');
    if fid == -1
      fprintf(2,'mcplot: Could not open file %s\n',object);
      fprintf(2,'mcplot: %s', err);
      return
    end
  end
  fclose(fid);
  %    opens filename and evaluate it
  object_orig = object;
  [pathname, object, ext]=fileparts(object);
  dorename = 0;
  if isempty(ext), dorename = 1;
  elseif ~strcmp(ext, '.m'), dorename = 1;
  end
  if dorename
    new_name = strrep(object_orig,'.','_');
    copyfile(object_orig, [ new_name '.m' ]);
    object = new_name;
  end
  filename = object;
  object = strrep(object,'.m','');
  if length(pathname)
    cur_dir = pwd;
    cd(pathname);  % go into directory where object is
    pathname = [ pathname filesep ];
  end
  m = [];
  m = eval(object,'[]');
  if ~length(m)
    disp(['mcplot: Could not extract McStas structure from file ' object]);
    disp( '        This is not a Matlab script (other format or binary ?)');
    disp(['mcplot: ' lasterr]);
    if dorename,delete([ new_name '.m' ]); end
    if length(pathname), cd(cur_dir); end
    return
  elseif dorename,delete([ new_name '.m' ]); end
  if length(m)
    object = m; clear m
  end
  if length(pathname), cd(cur_dir); end
end

% handles structure loading and ploting
if ~isstruct(object)
  disp('mcplot: Could not extract the McStas simulation structure.')
  return;
else  % if 's' is a 'struct'
  %    send to mcplot_scan(s, options)
  [count, object] = mcplot_scan(object, options, id);

  % set title to instrument+dirname+filename+parameters
  fud = get(gcf,'UserData');
  source   = eval('fud.instrument.Source','[]');
  if ~length(source), source   = eval('fud.Source','''McStas'''); end
  sdate    = eval('fud.Date','now');
  % The datestr call below is partly broken:
  % If sdate comes from the data file, we will not take offset
  % from GMT into account... Have made a query to comp.soft-sys.matlab
  % to ask for a solution...
  if ~ischar(sdate), sdate = datestr(sdate/60/60/24 + 719529); end
  ncount   = eval('fud.Ncount','''unknown''');
  overview = eval('fud.overview','[]');
  if ~length(filename) & isfield(object,'filename'), filename=object.filename; end
  if ~length(filename) & isfield(fud,'filename'),    filename=fud.filename; end
  if ~length(pathname) & isfield(object,'pathname'), pathname=object.pathname; end
  fud.pathname = pathname;
  set(gcf,'UserData',fud);
  t1 = [ '[' source '] ' pathname filename ];
  if length(overview), set(gcf,'Name',t1,'unit','pixel'); end
  % print simulation informations
  t2 = [ 'Ncount:' ncount '; Date: ' sdate ];
  if isfield(fud,'superdata')
        if isfield(fud.superdata, 'scannedvar')
        t2 = [ t2 '; Scan of ' fud.superdata.scannedvar '=' num2str(fud.superdata.minvar) ':' num2str(fud.superdata.maxvar) ' in ' num2str(fud.superdata.numpoints) ' points.'];
        end
  end
  parameters = eval('fud.parameters','[]');
  if ~isempty(parameters)
    t3 = '';
    % scan parameters structure, excluding 'class','parent','name'
    tmp_fields = fieldnames(parameters);
    for field=1:length(tmp_fields)
      switch tmp_fields{field}
      case {'class','parent','name'}
      otherwise
        t3 = [ t3 tmp_fields{field} '=' num2str(getfield(parameters, tmp_fields{field})) ' ' ];
      end
    end
  else
    t3 = 'unknown parameters';
  end

  % redimension all subplot axes in figure to make room for legend
  set(gcf,'unit','characters');
  posf  = get(gcf,'position');
  ratio = max(posf(end)-4,5)/posf(end);
  h = findobj(gcf,'type','axes');
  for index=1:length(h)
    pos = get(h(index),'position');
    pos = pos.*[1 ratio 1 ratio];
    pos(find(pos <= 0)) = 1e-3;
    set(h(index),'position',pos);
  end

  % create legend uicontrol
  pos = [ 1 posf(end)-4 posf(3) 4 ];
  NL = sprintf('\n');
  h = uicontrol('style','edit','max',4,'min',0,'unit','characters',...
        'position',pos,'string',{t1;t2;t3},...
        'ToolTipString',[ 'Simulation: ' t1 NL '  with ' t2 NL 'Parameters: ' t3]);
  set(h,'unit','normalized');

  % if it is a scan overview, add a menu item to call scan step selection
  if length(fud.overview) & isfield(fud,'superdata')
        h = findobj(gcf,'Tag','McStasMenu');
        uimenu(h,'Separator','on','Label','View scan step...','callback',['mcplot(''scanstep'',''action'', gcf);']);
  end
  set(gcf,'Toolbar','figure');  % make sure the tool bar is there

  %    if output is not empty, setup output file
  if length(findstr(options,'-gif'))
    disp('McPlot: GIF output not available, substituting with PNG.')
    options = strrep(options, '-gif','-png');
  end
  if length(findstr(options,'-ps')) ...
  | length(findstr(options,'-psc')) ...
  | length(findstr(options,'-eps')) ...
  | length(findstr(options,'-epsc')) ...
  | length(findstr(options,'-jpg')) ...
  | length(findstr(options,'-tif')) ...
  | length(findstr(options,'-png')) ...
  | length(findstr(options,'-fig')),
    filename = eval('object.File','[]');
    if length(filename) == 0, filename = eval('object.filename','[]'); end
    if length(filename) == 0, filename='mcstas'; end
    mcplot_output(options, gcf, filename);
  end
end
% end mcplot

% script for definition of colormaps and usefull functions for McStas/McPlot

% optional routines required by mcplot ---------------------------------------

function win = mcplot_addmenu()
% set up a mcplot local menu
  win = gcf;
  % remove McStas menu (if any)
  h = findobj(win, 'Tag', 'McStasMenu');
  if ~isempty(h), delete(h); end
  % creates a local McStas menu for looking at data files, and direct exporting
  h = uimenu(win, 'Label', '&McStas', 'Tag', 'McStasMenu');
  hcolor = uimenu(h, 'Label', '&Colors');
  hsave  = uimenu(h, 'Label', '&Save');
  hedit  = uimenu(h, 'Label', '&Edit');
  hdata  = uimenu(h, 'Label', '&Data','Tag','gcf_Data_menu');
  t = {'Save as &PNG', 'save_png', ...
    'Save as EPS (BW)', 'save_eps', ...
    'Save as &EPS (Color)', 'save_epsc', ...
    'Save as JPEG', 'save_jpg', ...
    'Save as TIFF', 'save_tif', ...
    'Save as Fig (Matlab)', 'save_fig', ...
    'Colormap &Jet', 'cmap_jet', ...
    'Colormap &HSV', 'cmap_hsv', ...
    'Colormap Hot (red)', 'cmap_hot', ...
    'Colormap Cool (&blue)', 'cmap_cool', ...
    'Colormap Gray', 'cmap_gray', ...
    'Colormap Pink', 'cmap_pink', ...
    'Colormap Inv. Pink', 'cmap_rpink', ...
    'Colormap : others...','set_color', ...
    'Edit &data file', 'edit_data', ...
    'Edit &instrument file', 'edit_instr', ...
    'View &the instrument','view_instr', ...
    'Add colorbar', 'add_colorbar', ...
    'Intensity: Linear scale', 'log_linear',...
    'Open McStas result file...', 'open_mcplot', ...
    'Exit...', 'exit', ...
    'About McStas...', 'about'};
  for index = 1:2:length(t) % setup the callbacks
    this_item = t{index};
    if length(strmatch(strtok(this_item),'Colormap')), sub_menu=hcolor;
    elseif length(strmatch(strtok(this_item),'Save')), sub_menu=hsave;
    elseif length(strmatch(strtok(this_item),'Edit')), sub_menu=hedit;
    else sub_menu=h; end
    uimenu(sub_menu, 'Label', this_item, ...
      'callback', ['mcplot(''' t{index+1} ''',''action'', gcf);']);
  end
% end mcplot_addmenu

function mcplot_menu_action(action, object)

  fud = get(gcf, 'UserData');
  if nargin == 1, object = ''; end
  if isempty(object), object = gco; end
  if isempty(object), object = gcf; end

  % extract global data
  filename = '';
  filename = eval('fud.filename','[]');

  switch action
    case 'set_color'  % Sets a colormap
      if any([strcmp(get(object,'Type'), 'surface') strcmp(get(object,'Type'), 'figure')])
        ColordListDlg =     {...
          'Default (usually Jet)',...
          'HSV (from red, throught yellow, green, cyan, blue, magenta, to red...)','Jet (from blue, through cyan, yellow, and orange, to red)',...
          'Autumn (red, through orange, to yellow)','Spring (shades of magenta and yellow)','Summer (shades of green and yellow)','Winter (shades of blue and green)',...
          'Bone (grayscale plus blue)','Copper (black to bright copper)','Gray (grayscale)','Pink (pastel shades of pink)',...
          'Cool (from cyan to magenta)','Hot (from black, through red, orange, and yellow, to white)',...
          'Flag (4 pure red, white, blue, and black)','Lines (8 regularly pure colors+gray)','Prism (6 regularly pure colors)','ColorCube (many regularly pure colors)'};
        % display colormap list
        [Selection, Ok] = listdlg('ListString', ColordListDlg, ...
                'SelectionMode', 'single', 'Name', 'Color map Selection (for surfaces)', ...
                'PromptString', 'Please select a ColorMap','ListSize', [300 270]);
        if (Ok == 1)
        % set colormaps on all Common.CurrentFigure
          ColorMap = ColordListDlg{Selection};
          ColorMap = lower(strtok(ColorMap));
          colormap(ColorMap);
        end
      elseif any(strcmp(get(object,'Type'), 'line'))
        if exist('scribelinedlg')
          scribelinedlg(object);
        else
          C =  uisetcolor(object, 'Set Plot Color');
          set(object, 'Color', C);
        end
      end
    case 'edit_data'  % Edit/_data file
      if ~length(filename)
        filename = eval('fud.filename','[]');
      end
      if length(filename), mcplot_edit_file(filename); end
    case 'duplicate'  % duplicate graph
      d=get(object,'UserData');
      fud0 = get(gcf, 'UserData');
      if ~isfield(d,'Source') & isfield(fud0,'Source'), d.Source = fud0.Source; end
      if ~isfield(d,'Ncount') & isfield(fud0,'Ncount'), d.Ncount = fud0.Ncount; end
      if ~isfield(d,'parameters') & isfield(fud0,'parameters'), d.parameters = fud0.parameters; end
      if ~isfield(d,'pathname') & isfield(fud0,'pathname'), d.pathname = fud0.pathname; end
      mcplot(d,'-plot');
    case 'edit_instr'  % Edit/instrument file
      filename = eval('fud.Source','[]');
      if length(filename), mcplot_edit_file(filename); end
    case 'view_instr'  % Plot existing .fig if available
      figname = eval('fud.Source','[]');
      % get the file name without the path and the extension
      [tmp_path, figname] = fileparts(figname);
      if ~exist([figname '.fig'],'file')
        parameters = eval('fud.parameters','[]');
        if ~isempty(parameters)
          % scan parameters structure, excluding 'class','parent','name'
    if ispc
      tmp_parcmd = [ '!mcdisplay.pl -pMatlab --save ' figname '.exe ' ];
    else
            tmp_parcmd = [ '!mcdisplay -pMatlab --save ./' figname '.out ' ];
    end
          tmp_fields = fieldnames(parameters);
          for field=1:length(tmp_fields)
            switch tmp_fields{field}
            case {'class','parent','name'}
            otherwise
              tmp_parcmd = [ tmp_parcmd ' ' tmp_fields{field} '=' num2str(getfield(parameters, tmp_fields{field})) ];
            end
          end
          % launch the mcdisplay process to generate the instr view as a fig file
          fprintf(1,'Executing:%s\n',tmp_parcmd);
          eval(tmp_parcmd,'[]');
        end
      end
      if exist([figname '.fig'])==2, openfig([figname '.fig'],'reuse'); end
    case 'add_colorbar'  % Add _colorbar
      colorbar;
    case 'del_colorbar'  % Add _colorbar
      delete(colorbar);
    case 'add_text'   % add text
      t = 'Enter the text to add, and click the OK button';
      setstatus(t); fprintf(1,'%s\n',t);
      answer = inpudlg(['Text to add to Figure ' num2str(gwin)],'McPlot: add text');
      if length(answer)
        t = 'Select the position where to paste the text.';
        setstatus(t); fprintf(1,'%s\n',t);
        gtext(answer);
        setstatus('Ready');
      end
    case 'save_png'  % Save as/_GIF
      mcplot_output('-png',[],filename);
    case 'save_eps'  % Save as/EPS (b-w)
      mcplot_output('-eps' ,[],filename);
    case 'save_epsc'  % Save as/EPS (color)
      mcplot_output('-epsc',[],filename);
    case 'save_jpg'  % Save as/JPEG
      mcplot_output('-jpg',[],filename);
    case 'save_tif'  % Save as/TIFF
      mcplot_output('-tif',[],filename);
    case 'save_fig'  % Save as/Matlab
      mcplot_output('-fig',[],filename);
    case 'cmap_jet'  % Colormap/_Jet
      colormap(jet);
    case 'cmap_hsv'  % Colormap/HSV
      colormap(hsv);
    case 'cmap_hot'  % Colormap/hot
      colormap(hot);
    case 'cmap_cool'  % Colormap/cool
      colormap(cool);
    case 'cmap_gray'  % Colormap/gray
      colormap(gray);
    case 'cmap_pink'  % Colormap/pink
      colormap(pink);
    case 'cmap_rpink'  % Colormap/inverted pink
      colormap(1-pink);
    case 'scanstep'    % opens a list dialog for a Scan Step selection to open
      pathname = eval('fud.pathname','[]');
      filename = eval('fud.filename','[]');
      source   = eval('fud.Source','[]');
      ncount   = eval('fud.Ncount','''unknown''');
      if isfield(fud,'superdata')
        if isfield(fud.superdata, 'scannedvar')
          % make a list of scan step items
          index = cellstr(num2str(transpose(0:(fud.superdata.numpoints-1))));
          scannedvar = linspace(fud.superdata.minvar, fud.superdata.maxvar, fud.superdata.numpoints);
          scannedvar = cellstr(num2str(transpose(scannedvar)));
          scannedvar = strcat('[#', index, '] ', [ fud.superdata.scannedvar '=' ], scannedvar);
          prompt = {'Select scan step(s) open for',[ '[' source '] ' pathname filename ], ...
          [ 'Scan of ' fud.superdata.scannedvar '=' num2str(fud.superdata.minvar) ':' num2str(fud.superdata.maxvar) ' in ' num2str(fud.superdata.numpoints) ' points.']};
          selection = listdlg('ListString',scannedvar, 'SelectionMode','multiple',...
                  'PromptString', prompt, 'ListSize', [200 100], ...
                  'Name',[ '[' source '] Select scan step for ' filename ]);
          if ~isempty(selection)
            for index=1:length(selection)
              disp([ 'mcplot(''' deblank([ pathname num2str(selection(index)-1) ]) ''',''-overview'');' ]);
              figure;
              mcplot(deblank([ pathname num2str(selection(index)-1) ]),'-overview');
            end
          end
        end
      end
    case 'log_linear' % toggle linear/log scale
      if length(findstr(get(gcbo,'Label'),'Log'))
        set(gcbo,'Label', 'Intensity: Linear scale');
        scale = 'linear';
      else
        set(gcbo,'Label', 'Intensity: Log scale');
        scale = 'log';
      end
      all_axes = findobj(gcf, 'Type','axes');
      for index_a = 1:length(all_axes)
        all_plots = get(all_axes(index_a), 'Children');
        for index_p = 1:length(all_plots)
          this_child = all_plots(index_p);
    if strcmp(get(this_child, 'Type'), 'line') | strcmp(get(this_child, 'Type'), 'hggroup')
            set(all_axes(index_a), 'YScale', scale);
          elseif strcmp(get(this_child, 'Type'), 'surface')
            set(all_axes(index_a), 'ZScale', scale);
      if strcmp(scale,'log')
        zdat=get(this_child,'zdata');
              cdat=log(zdat);
              set(this_child,'cdata',cdat)
            else
              zdat=get(this_child,'zdata');
              set(this_child,'cdata',zdat)
            end
          end
        end
      end
    case 'exit'  % Close all
        exit
    case 'about'
      t = {'McStas McPlot menu',...
        '', ...
        'This is the McStas McPlot tool menu',...
        'It enables to customize the plot aspects and colors,'...
        'add objects, as well as export in various formats.', ...
        '','Use contextual menus on lines and surfaces to access',...
        'additional functionalities',...
        '','Please visit <http://www.mcstas.org/>','', ...
        'McStas comes with ABSOLUTELY NO WARRANTY',...
        'This is free software, and you are welcome',...
        'to redistribute it under certain conditions',...
        'as specified in Licence files.'};
      msgbox(t, 'McPlot: About','help','non-modal');
    case 'open_mcplot'
      figure;
      mcplot('open_mcplot_fileselector','overview');
    end
% end mcplot_menu_action

function mcplot_edit_file(filename)
% edit a file using either the EDITOR variable, or default editor

  if length(filename)
      if ~exist(filename, 'file'), filename = [ '..' filesep filename]; end % try one level up
      if ~exist(filename, 'file'), filename = [ '..' filesep filename]; end % try two levels up
      if exist(filename, 'file')
        t = ['McPlot: Editing file ' filename];
        setstatus(t); fprintf(1,'%s\n',t);
        edit(filename);
      end
    end
% end mcplot_edit_file

function mcplot_output(form, win, filename)
% output the current graphic window in the specified format (default is GIF)
% format may be: gif, ps, psc, fig, scilab
  if nargin == 0, form='GIF'; end
  if nargin <= 1, win = -1; end
  if nargin <= 2, filename=''; end
  if length(win) == 0, win = -1; end
  if win < 0,         win = gcf; end
  if length(filename) == 0, filename='mcstas'; end
  form = lower(form);

  ext = ''; dr = '';
  if length(findstr(form,'-gif'))
    disp('McPlot: GIF output not available, substituting with PNG.')
    form = strrep(form, '-gif','-png');
  end
  %    if output is not empty, open driver+xinit(filename)
  if     length(findstr(form,'-ps')),  ext = '.ps';  dr='-dps'; colormap(gray);
  elseif length(findstr(form,'-psc')), ext = '.ps';  dr='-dpsc';
  elseif length(findstr(form,'-eps')), ext = '.eps'; dr='-deps'; colormap(gray);
  elseif length(findstr(form,'-epsc')),ext = '.eps'; dr='-depsc';
  elseif length(findstr(form,'-jpg')), ext = '.jpg'; dr='-djpeg';
  elseif length(findstr(form,'-tif')), ext = '.tif'; dr='-dtiff';
  elseif length(findstr(form,'-png')), ext = '.png'; dr='-dpng';
  elseif length(findstr(form,'-fig')), ext = '.fig'; dr=''; end
  if length(ext)
    filename = [ filename ext ];
    if ~strcmp(ext,'.fig')
      print(gcf, dr, filename);
    else saveas(gcf, filename, 'fig');
    end
    t=['mcplot: McStas plot exported as file ' filename ' (' form ')'];
    setstatus(t); fprintf(1,'%s\n',t);
  end
% end mcplot_output

% basic routines required by mcplot ------------------------------------------

function d=mcplot_load(d)
% local inline function to load data
S=d.type;
StartIdx=findstr('(',S)+1;
eval(['S=[ ' S(StartIdx:(length(S)-1)) ' ];']);
if isempty(d.data)
 if ~length(findstr(d.format, 'binary'))
  if ~strcmp(d.filename,[d.func,'.m']) copyfile(d.filename,[d.func,'.m']); end
  p=d.parent;path(path);
  eval(['d=',d.func,';']);d.parent=p;
  if ~strcmp(d.filename,[d.func,'.m']) delete([d.func,'.m']); end
 else
  if length(findstr(d.format, 'float')), t='single';
  elseif length(findstr(d.format, 'double')), t='double';
  else return; end
  if length(S) == 1, S=[S 1]; end
  fid=fopen(d.filename, 'r');
  pS = prod(S);
  x = fread(fid, 3*pS, t);
  d.data  =reshape(x(1:pS), S);
  if prod(size(x)) >= 3*pS,
  d.errors=reshape(x((pS+1):(2*pS)), S);
  d.events=reshape(x((2*pS+1):(3*pS)), S);end
  fclose(fid);
 end
 if length(strindex(d.type,'1d'))
  if size(d.data,2) > 1 & size(d.errors,2) ==0
   d.errors = d.data(:,2);
   d.data   = d.data(:,1);
  end
 end
end

function d=mcplot_plot(d,p)
  % func to plot data
  if isempty(findstr(d.type,'0d')), d=mcplot_load(d); end
  if ~length(d.values), d.values = [ num2str(sum(sum(d.data)),'%.4g') ' ' num2str(sum(sum(d.errors)),'%.4g') ]; end
  if ~length(d.signal), d.signal = [ 'Min=' num2str(min(min(d.data)),'%.4g') '; Max=' num2str(max(max(d.data)),'%.4g') '; Mean=' num2str(mean(mean(d.data)),'%.4g') ]; end
  if ~p, return; end;
  eval(['l=[',d.xylimits,'];']);
  S=size(d.data);
  t1=['[',d.parent,'] ',d.filename];
  t = strvcat(t1,['  ',d.variables,'=[',d.values,']'],['  ',d.signal],['  ',d.statistics]);
  disp(t);
  if ~isempty(findstr(d.type,'0d')),return;
  else
    if p==1 & ~isempty(findobj(gcf, 'Type','axes'))
      w=figure;
    else w = gcf; end
    if ~isempty(findstr(d.type,'2d'))
      if S(2) > 1, d.stepx=abs(l(1)-l(2))/(S(2)-1); else d.stepx=0; end
      if S(1) > 1, d.stepy=abs(l(3)-l(4))/(S(1)-1); else d.stepy=0; end
      d.x=linspace(l(1)+d.stepx/2,l(2)-d.stepx/2,S(2));
      d.y=linspace(l(3)+d.stepy/2,l(4)-d.stepy/2,S(1)); z=d.data;
      h=surface(d.x,d.y,d.data);
      xlim([l(1) l(2)]); ylim([l(3) l(4)]);
      shading flat;
    elseif ~isempty(findstr(d.type,'1d')) & isfield(d, 'errors')
      if max(S) > 1, d.stepx=abs(l(1)-l(2))/(max(S)-1);
      else d.stepx=0; end
      d.x=linspace(l(1),l(2),max(S));
      h=errorbar(d.x,d.data,d.errors);
      xlim([l(1) l(2)]);
    else
      if max(S) > 1, d.stepx=abs(l(1)-l(2))/(max(S)-1);
      else d.stepx=0; end
      d.x=linspace(l(1),l(2),max(S));
      h=plot(d.x,d.data);
      xlim([l(1) l(2)]);
    end
    d.title = t;
    set(h, 'UserData', d,'Tag',[ 'mcplot_data_' d.filename ]);
    hm = uicontextmenu;
    uimenu(hm, 'Label',['Duplicate ' d.filename], 'Callback', ...
      'mcplot(''duplicate'', ''action'', gco);');
    uimenu(hm, 'Label',['Edit ' d.filename], 'Callback', ...
      [ 'mcplot(''edit_data'', ''action'', gco);' ]);
    uimenu(hm, 'Label','Select Color', 'Callback', ...
      [ 'mcplot(''set_color'', ''action'', gco);' ]);
    uimenu(hm, 'Label','Reset View', 'Callback','view(0,90);lighting none;alpha(1);');
    if ~isempty(findstr(d.type,'2d'))
        uimenu(hm, 'Label','Add Light','Callback', 'light;lighting phong;');
        uimenu(hm, 'Label','Transparency','Callback', 'alpha(0.5);');
    end
    uimenu(hm, 'Label',['Export into mc_' d.filename ],'Callback', [ 'evalin(''base'',''mc_' d.filename ' = get(gco,''''userdata''''); disp([''''Exported data into variable mc_' d.filename ''''']);'');'])
    uimenu(hm, 'Label',['Properties of ' d.filename ],'Callback', [ 'h=get(gco,''userdata''); msgbox(h.title,h.title(1,:),''help'');' ])
    fud = get(gcf,'UserData');
    if isfield(fud,'superdata')
        uimenu(hm,'Separator','on','Label','View scan step...','callback',['mcplot(''scanstep'',''action'', gcf);']);
    end
    set(h, 'UIContextMenu', hm);
    % Also set this UIContextMenu on the axis...
    % Could also be done using gca as handle, but
    % can break in certain setups...
    for j=1:length(h)
      set(get(h(j),'parent'), 'UIContextMenu', hm, 'userdata',get(h(j),'userdata'));
    end
    % add data set to mcstas/Data menu
    hdata = findobj(gcf,'Tag','gcf_Data_menu');
    if ~isempty(hdata)
      uimenu(hdata,'Label',d.filename,'Callback',[ 'h=findobj(''Tag'',''mcplot_data_' d.filename '''); if ~isempty(h), mcplot(''duplicate'', ''action'',h); end' ]);
    end
  end
  if p == 2, t = d.filename; end
  xlabel(d.xlabel); ylabel(d.ylabel); h=title(t); set(h,'interpreter','none');
  if p==1, set(gca,'position',[.18,.18,.7,.65]);  end
  set(gcf,'name',t1);grid on;
  % if ~isempty(findstr(d.type,'2d')), colorbar; end
% end mcplot_plot

function mcplot_set_global(s, gwin, p_in)

  if length(p_in) == 3
    m = p_in(1);
    n = p_in(2);
    p = p_in(3);
    p_in = p;
  else m=0; n=0; p=p_in; end
  if ~p, p=1; end

  if ~length(gwin), gwin = gcf; end
  ThisFigure = get(gcf,'UserData');

  if ~length(ThisFigure)
    filename = eval('s.File','[]');
    if length(filename) == 0, filename = eval('s.filename','[]'); end
    if length(filename), ThisFigure.filename = filename; end
    ThisFigure.overview  = [];
  end
  if isfield(s,'Source'), ThisFigure.Source     = s.Source; end
  if isfield(s,'Date'), ThisFigure.Date     = s.Date; end
  if isfield(s,'Ncount'), ThisFigure.Ncount     = s.Ncount; end
  if isfield(s,'parameters'), ThisFigure.parameters = s.parameters; end
  if strcmp(s.class,'instrument')
    instrument.Source     = s.Source;
    instrument.name       = s.name;
    instrument.Parameters = s.Parameters;
    instrument.class = 'instrument';
    ThisFigure.instrument = instrument;
  elseif strcmp(s.class,'simulation')
    [a,b,ext] = fileparts(s.name);
    if ~strcmp(ext,'.m')
        ThisFigure.filename = [s.name '.m'];
    else ThisFigure.filename = s.name; end
  elseif strcmp(s.class,'parameters')
    ThisFigure.parameters = s;
  elseif strcmp(s.class,'data')
    if ~p_in, ThisFigure.filename = s.filename; end
    if isfield(s,'ratio') & ~isfield(ThisFigure,'Ncount')
        ThisFigure.Ncount = s.ratio; end
  elseif strcmp(s.class,'superdata')
    ThisFigure.superdata = s;
  end % ignore other classes

  if m*n, ThisFigure.overview = [m n p]; end

  % store Figure McPlot info
  set(gcf,'UserData', ThisFigure);
% end mcplot_set_global

function [data_count, s] = mcplot_scan(s,action, m,n,p, id)
% scans the structure s recursively and opens a graphic window
% for all found 'data' structures having curves/surfaces data.
% input: s:      structre where we look for data blocks
%        action: may be 'count', '-plot' and '-overview'
%        m,n,p: indexes for subplot windows
% m may also be used as a string keyword used for searching within McStas
%  structure fields 'filename','title', 'type'

  if nargin == 0, data_count = 0; return; end
  if ~isstruct(s), data_count = 0; return; end
  tag_names = fieldnames(s);
  if length(strmatch('class', char(tag_names))) == 0
    if length(strmatch('data', char(tag_names))), s.class = 'data';
    else
      s.class = 'root';
      data_count = 0;
    end
  end
  if nargin == 1, action = ''; end
  if nargin == 3, id = m; end
  if ~length(action), action = '-overview'; end
  if length(findstr(action,'-overview')) & nargin == 3
    % first count the number of data blocks
    data_count = mcplot_scan(s,'count',id);
    if ~data_count, return; end
    m = floor(sqrt(data_count));
    n = ceil(data_count/m);
    p = 1;
    subplot(m,n,p);
    mcplot_addmenu;
    data_count = 0;
  elseif nargin == 3,
    m=0; n=0; p=0;
    data_count = p;
  else data_count = p;
  end
  if ~strcmp(s.class,'data')
    if strcmp(s.class,'parameters') | strcmp(s.class,'simulation') | ...
       strcmp(s.class,'instrument') | strcmp(s.class,'superdata')
      mcplot_set_global(s, [], 0);
    end
    for i=1:max(size(tag_names))
      d = getfield(s,tag_names{i});
      if isstruct(d)
        [ndc, d]   = mcplot_scan(d,action,m,n,data_count,id);
        data_count = ndc;
        if length(d) > 0
          setfield(s, tag_names{i}, d);
        end
      end
    end
  else
    doplot = 1;
    if length(id)
      if ~length(findstr(s.filename,id)) & ~length(findstr(s.title,id)) & ~length(findstr(s.type,id))
        doplot = 0;
      end
    end
    if doplot
      if length(findstr(action,'count'))
        s = mcplot_plot(s, 0);
      elseif length(findstr(action,'-plot'))
        s = mcplot_plot(s, 1);
        mcplot_set_global(s, [], 0);
        mcplot_addmenu;
      elseif length(findstr(action,'-overview'))
        subplot(m,n,data_count+1);
        s = mcplot_plot(s, 2);
        mcplot_set_global(s, [], [m,n,data_count+1]);
      end
      data_count = data_count+1;
    end
  end % else class == 'data'
% end mcplot_scan



function [success, OSMessage]=copyfile(src,dest)
% Overload of Matlab's internal copyfile function, which
% is buggy in the 6.5 release. Since usage of the function
% is limited, we simply use our own in all cases.
  if ispc
    [Status, OSMessage] = dos(['copy ' src ' '  dest]);
  elseif isunix
    [Status, OSMessage] = unix(['cp -r ' src ' ' dest]);
  end
  success = ~Status;
% end copyfile
