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

% parameter check
if nargin == 0, object=''; end
if nargin <= 1, options=''; end
if nargin <= 2, id = ''; end

if ~ischar(options),  options = ''; end
if ~length(options), options = '-overview'; end
options = lower(options);  % to lower case
if strcmp(options,'action')
  mcplot_menu_action(object, id);
  return
end

% handle file name specification in 'object'
if ischar(object) % if object is a string
  if size(object,1) > 1
    for index=1:size(object,2)
      [object,count] = mcplot(object(index), options, id);
    end
    return
  end
  % if object is a '' string, s = 'mcstas'.
  if length(object) == 0, object = 'mcstas.m'; end
  [fid, err] = fopen(object, 'r');
  if fid == -1 % error occured. Calls fileselector (uigetfile)
    [object, pathname] = uigetfile('*.m', 'Select a McStas simulation file to load');
    if ~ischar(object), return; end
    [fid, err] = fopen(object, 'r');
    if fid == -1
      fprintf(2,'mcplot: Could not open file %s\n',object);
      fprintf(2,'mcplot: %s', err);
      return
    end
  end
  fclose(fid);
  %    opens filename and evaluate it
  object = strrep(object,'.m','');
  [pathname, object]=fileparts(object);
  if not(isempty(pathname))
    cd(pathname);  % go into directory where object is
  end
  m = [];
  m = eval(object,'[]');
  if ~length(m)
    disp(['mcplot: Could not extract McStas structure from file ' object]);
    disp(['mcplot: ' lasterr]);
    return
  end
  if length(m)
    object = m; clear m
  end
end

% handles structure loading and ploting
if ~isstruct(object)
  disp('mcplot: Could not extract the McStas simulation structure.')
  return;
else  % if 's' is a 'struct'
  %    send to mcplot_scan(s, options)
  [count, object] = mcplot_scan(object, options, id);
  %    if output is not empty, setup output file
  if length(findstr(options,'-ps')) ...  
  | length(findstr(options,'-psc')) ... 
  | length(findstr(options,'-eps')) ... 
  | length(findstr(options,'-epsc')) ...
  | length(findstr(options,'-jpg')) ... 
  | length(findstr(options,'-tif')) ... 
  | length(findstr(options,'-png')) ... 
  | length(findstr(options,'-fig')), 
    filename = '';
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
  t = {'Edit &data file', 'edit_data', ... 
    'Edit &instrument file', 'edit_instr', ...                
    'View &the instrument','view_instr', ...
    'Add &colorbar', 'add_colorbar', ...  
    'Save as &PNG', 'save_png', ...           
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
    'Exit...', 'exit', ...
    'About McStas...', 'about'};
  for index = 1:2:length(t) % setup the callbacks
    uimenu(h, 'Label', t{index}, ...
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
      if any(strcmp(get(object,'Type'), 'surface'))
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
        filename = eval('fud.simulation','[]');
      end
      if length(filename), mcplot_edit_file(filename); end
    case 'edit_instr'  % Edit/instrument file
      filename = eval('fud.instrument.Source','[]');
      if length(filename), mcplot_edit_file(filename); end
    case 'view_instr'  % Plot existing .fig if available
      figname = [eval('fud.instrument.name','[]') '.fig'];
      if exist(figname)==2 openfig(figname,'reuse'); end
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
    case 'exit'  % Close all
      Stop=questdlg('Exit Matlab/McPlot ?','McPlot: Exit ?','Yes','No','Yes');
      if strcmp(Stop,'Yes')
  	    exit
      end
    case 'about' 
      t = {'McStas McPlot menu',...
        '', ...
        'This is the McStas McPlot tool menu',...
        'It enables to customize the plot aspects and colors,'...
        'add objects, as well as export in various formats.', ...
        '','Please visit <http://www.neutron.risoe.dk/mcstas>','', ...
        'McStas comes with ABSOLUTELY NO WARRANTY',...
        'This is free software, and you are welcome',...
        'to redistribute it under certain conditions',...
        'as specified in Licence files.'};
      msgbox(t, 'McPlot: About','help','non-modal');      
    end 
% end mcplot_menu_action  

function mcplot_edit_file(filename)
% edit a file using either the EDITOR variable, or default editor

  if length(filename)
      if ~exist(filename, 'file'), filename = [ '..' filesep filename]; end % try one level up
      if ~exist(filename, 'file'), filename = [ '..' filesep filename]; end % try one level up
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
S=d.type; eval(['S=[ ' S(10:(length(S)-1)) ' ];']);
if isempty(d.data)
 if ~length(findstr(d.format, 'binary'))
  copyfile(d.filename,[d.func,'.m']);p=d.parent;
  path(path);
  eval(['d=',d.func,';']);d.parent=p;delete([d.func,'.m']);
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
  return
 end
end

function d=mcplot_plot(d,p)
  % func to plot data
  if isempty(findstr(d.type,'0d')), d=mcplot_load(d); end
  if ~p, return; end;
  eval(['l=[',d.xylimits,'];']);
  S=size(d.data);
  t1=['[',d.parent,'] ',d.filename,': ',d.title];
  t = strvcat(t1,['  ',d.variables,'=[',d.values,']'],['  ',d.signal],['  ',d.statistics]);
  disp(t);
  if ~isempty(findstr(d.type,'0d')),return;
  else
    if p==1 & ~isempty(findobj(gcf, 'Type','axes'))
      w=figure;
    else w = gcf; end
    if ~isempty(findstr(d.type,'2d'))
      d.x=linspace(l(1),l(2),S(2)); 
      d.y=linspace(l(3),l(4),S(1));
      h=surface(d.x,d.y,d.data);           
    else
      d.x=linspace(l(1),l(2),max(S));
      h=plot(d.x,d.data);
    end
    set(h, 'UserData', d);
    hm = uicontextmenu;
    uimenu(hm, 'Label',['Duplicate ' d.filename], 'Callback', ...
      'd=get(gco,''UserData''); mcplot(d,''-plot'');');
    uimenu(hm, 'Label',['Edit ' d.filename], 'Callback', ...
      [ 'mcplot(''edit_data'', ''action'', gco);' ]);
    uimenu(hm, 'Label','Select Color', 'Callback', ...
      [ 'mcplot(''set_color'', ''action'', gco);' ]);
    uimenu(hm, 'Label','Reset View', 'Callback','view(0,90);lighting none;alpha(1);');
    uimenu(hm, 'Label','Add Light','Callback', 'light;lighting phong;');
    uimenu(hm, 'Label','Transparency','Callback', 'alpha(0.5);');
    uimenu(hm, 'Label',['Export into ' d.filename ],'Callback', [ 'evalin(''base'',''' d.filename ' = get(gco,''''userdata''''); disp([''''Exported data into variable ' d.filename ''''']);'');'])
    set(h, 'UIContextMenu', hm);
  end
  if p == 2, t = t1; end
  xlabel(d.xlabel); ylabel(d.ylabel); title(t);
  axis tight;
  if p==1, set(gca,'position',[.18,.18,.7,.65]);  end
  set(gcf,'name',t1);grid on;
  % if ~isempty(findstr(d.type,'2d')), colorbar; end
% end mcplot_plot

function mcplot_set_global(s, gwin, p_in)

  p = p_in;
  if ~p, p=1; end
  
  if ~length(gwin), gwin = gcf; end
  ThisFigure = get(gcf,'UserData');

  if ~length(ThisFigure)
    filename = eval('s.File','[]');
    if length(filename) == 0, filename = eval('s.filename','[]'); end
    if length(filename), ThisFigure.simulation = filename; end
  end
  if strcmp(s.class,'instrument')
    instrument.Source     = s.Source;
    instrument.name       = s.name;
    instrument.Parameters = s.Parameters;
    instrument.class = 'instrument';
    ThisFigure.instrument = instrument;
  elseif strcmp(s.class,'simulation')
    ThisFigure.simulation            = [s.name '.m'];
  elseif strcmp(s.class,'parameters')
    ThisFigure.parameters = s;
  elseif strcmp(s.class,'data')
    if ~p_in, ThisFigure.filename = s.filename; 
    else ThisFigure.filename = ThisFigure.simulation; end
  end % ignore other classes

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
  if length(strmatch('class', tag_names)) == 0
    if length(strmatch('data', tag_names)), s.class = 'data'; 
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
    if strcmp(s.class,'parameters') | strcmp(s.class,'simulation') | strcmp(s.class,'instrument')
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
        mcplot_set_global(s, [], data_count+1);
      end
      data_count = data_count+1; 
    end
  end % else class == 'data'
% end mcplot_scan



