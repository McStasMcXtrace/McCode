// mcplot: plot a McStas simulation result
// [object,count]=mcplot(object, options, id)
//
// This function displays a McStas simulation result either as many windows
// or on a single window with subplots. It also returns the McStas simulation 
// structure. An 'id' may be specified for filtering names within structure.
//
// input:
//  object: one or more simulation name(s) or a single mcstas structure
//          or a single detector file name
//          if filename does not exist, a file selector is called.
//  options may contain keywords 
//    '-overview' to plot all results on the same window
//    '-plot'   to plot all results in separate windows
//    '-gif'    to export as a GIF file
//    '-ps'     to export as a PostScript file
//    '-psc'    to export as a color PostScript file
//    '-fig'    to export as an XFig file
//    '-ppm'    to export as a PPM file
//    '-scg'    to export as a Scilab figure (xload/xsave)
// id is a keyword used for searching within McStas structure fields
//    'filename' and 'title'
//
// examples:
//   mcplot();
//   s=mcplot('ask me the file','-overview')
//   mcplot(s, 'plot -gif', 'MyMonitorName');
//
// Written by: E. Farhi
// Date: 21st, March 2003
// Release: McStas 1.6
// Origin: ILL

// script for definition of colormaps and usefull functions for McStas/McPlot

// optional routines required by mcplot ---------------------------------------

function j=jet()
// jet colormap (approximation)
  c=[linspace(0,1,15), ones(1,15), linspace(1,0,15)];
  r=[(1:24)*0,c(1:($-5))];
  g=[(1:8)*0,c,(1:11)*0];
  b=[c(6:$),(1:24)*0];
  j=[r' g' b'];
  xset('colormap',j);
endfunction

function h=hsv()
// hsv colormap (approximation)
  c=[linspace(0,1,12), ones(1,20), linspace(1,0,12)];
  r=[ones(1,10),1-c,ones(1,10)];
  g=[c,(1:20)*0];
  b=[(1:20)*0,c];
  h=[r' g' b'];
  xset('colormap',h);
endfunction

function h=hot()
  h=hotcolormap(64);
  xset('colormap',h);
endfunction

function h=cool()
  h=1-hotcolormap(64);
  xset('colormap',h);
endfunction

function g=gray()
  g=graycolormap(64);
  xset('colormap',g);
endfunction

function p=pink()
  p=sqrt((2*graycolormap(64) + hotcolormap(64))/3);
  xset('colormap',p);
endfunction

function p=rpink()
  p=1-sqrt((2*graycolormap(64) + hotcolormap(64))/3);
  xset('colormap',p);
endfunction

function win = mcplot_addmenu(use_common_menu)
// set up a mcplot local menu

  global MCPLOT

  win = xget('window');
  if argn(2) > 1 & MCPLOT.MenuInstalled, return; end
  
  // remove McStas menu (if any)
  delmenu(win,'McStas')
  // creates a local McStas menu for looking at data files, and direct exporting
  menu_installed = 0;
  t = [ 'Select a figure', ...
        'Open in a separate window', ...      
        'Edit/_data file', ... 
        'Edit/_instrument file', ... 
        'Edit/colormap...', ...               
        'Edit/preferences...', ...       
        'View instrument...', ...
        'Reset Top-View', ...
        'Add/_colorbar', ...  
        'Add/_text...', ...       
        'Save as/_GIF', ...           
        'Save as/EPS (BW)', ...     
        'Save as/_EPS (Color)', ...   
        'Save as/Fig (Xfig)', ...    
        'Save as/PPM', ...    
        'Save as/Scg (Scilab fig)', ...  
        'Colormap/_Jet', ...                  
        'Colormap/_HSV', ...                  
        'Colormap/Hot (red)', ...            
        'Colormap/Cool (_blue)', ...          
        'Colormap/Gray',...
        'Colormap/_Pink',...
        'Colormap/Inv. Pink',...
        'About McStas...',...
        'Exit'];	// exit must be last choice
  if exists('with_gtk')
    if with_gtk() // with scilab 2.7 on Linux with GTK+ libs
      addmenu(win, '_McStas', t, list(2,'mcplot_menu_action'));
      menu_installed = 1;
      MCPLOT.MenuInstalled = 1;
    end
  end
  if ~menu_installed & ~MCPLOT.MenuInstalled
    t = strsubst(t, '_', '');
    t = strsubst(t, '/', ' ');
    if havewindow() & with_tk()
      // we may use Tk external menu
      MCPLOT.MenuInstalled = 1;
            
      // build-up the Tcl/Tk string for Tk_EvalStr
      tcl_script=['set w .foo',...
                  'catch {destroy .foo}',...
                  'toplevel .foo',...
                  'wm title .foo ""McStas/McPlot menu""',...
                  'frame .foo.menu -relief raised -borderwidth 2',...
                  'pack .foo.menu -side top -fill x',...
                  'menubutton .foo.menu.mcplot -text ""McStas/McPlot"" -menu .foo.menu.mcplot.m -underline 0',...
                  'menu .foo.menu.mcplot.m'];
      for index=1:size(t,2)
        tcl_script = [ tcl_script, ...
          '.foo.menu.mcplot.m add command -label ""'+t(index)+'"" -underline 0  -command {ScilabEval ""mcplot_menu_action('+string(index)+')""}'];
	  
      end
      tcl_script = [ tcl_script, 'pack .foo.menu.mcplot -side left' ];
      // now send it to Tk
      TK_EvalStr(tcl_script);
    elseif argn(2) > 0
      // we shall use x_choices
      rep = x_choices(['Fig '+string(win)+': Choose something to do','Select ""Exit"" to delete this dialog'], list(list('Action:', 1, t)));
      if length(rep), mcplot_menu_action(rep); end
      if rep == size(t,2), win=0; else win = -1; end
    end
  end
endfunction // mcplot_addmenu

function mcplot_menu_action(k, gwin)

  global MCPLOT
  
  if argn(2) == 1
    gwin = xget('window');
  end
  
  // extract global data
  ThisFigure = [];
  execstr('ThisFigure = MCPLOT.Figure_'+string(gwin),'errcatch');
  filename = '';
  execstr('filename = ThisFigure.filename','errcatch');
  
  if argn(2) == 0, k = 1; end
  if MCPLOT.ShiftedItems
    k = k+1;
  end
  if k <= 0
    mprintf('Oops, the Tcl/Tk uses shifted menu item indexes\n');
    mprintf('This should be corrected now...a\n');
    MCPLOT.ShiftedItems = 1;
    k=1;
  end
  
  xset('window', gwin); // raise menu activated window 
  item = [ 24, 1, 2, 19, 3, 4, 23, 25, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 22, 18]
  
  select item(k)
    case 0 then
    	mprintf('Invalid menu item\n');
    case 1 then // Open in a separate window
      t='Figure '+string(gwin)+': Click on the plot to duplicate/enlarge.';
      xinfo(t); mprintf('%s\n',t);
      [c_i, c_x, c_y] = xclick();
      [wrect,frect,logflag,arect] = xgetech();
      if ~length(find(arect~=0)), arect=[1,1,1,1]/8; end
      // compute the mouse click coordinates in subplot units (m,n)
      // m/n=0 on lower left, 1 on upper right of current axis (in frame)
      m = (c_x-frect(1))/(frect(3)-frect(1));
      n = (c_y-frect(2))/(frect(4)-frect(2));
      // invert n to 1-n, for wrect usage
      // convert to coords of current whole frame
      m = m*(1-arect(1)-arect(2))+arect(1);
      n = (1-n)*(1-arect(3)-arect(4))+arect(3);
      // m=0/1 for left/right of window
      // n=0/1 for bottom/top of window
      m = m*wrect(3)+wrect(1);  
      n = n*wrect(4)+wrect(2);
      // compute m,n of subplot from coordinates
      i = floor(m/wrect(3)); m = round(1/wrect(3));
      j = floor(n/wrect(4)); n = round(1/wrect(4));
      i = min(i,m-1); j=min(j,n-1);
      i = max(i,0);   j=max(j,0);
      p = i+j*m+1;  // single subplot index
      t = msprintf('Duplicating Figure %d:subplot %d', gwin, p);
      // extract global data
      d = [];
      execstr('d = ThisFigure.Subplot_'+string(p),'errcatch');
      if length(d)
        mcplot_scan(d,'-plot',''); 
        nwin = xget('window'); // new opened window
        // update the MCPLOT global variable
        execstr('MCPLOT.Figure_'+string(nwin)+'.simulation = ThisFigure.simulation;','errcatch');
        d = []; execstr('d = ThisFigure.parameters;','errcatch');
        if length(d), mcplot_set_global(d, nwin, 0); end
        d = []; execstr('d = ThisFigure.instrument;','errcatch');
        if length(d), mcplot_set_global(d, nwin, 0); end
	xclear();
      	xtape('replayna', nwin, 90, 0);	// top view
      end
      xinfo(t); mprintf('%s\n',t);
    case 2 then // Edit/_data file
      if ~length(filename)
        execstr('filename = ThisFigure.simulation','errcatch');
      end
      if length(filename), mcplot_edit_file(filename); end
    case 19 then // Edit/instrument file
      execstr('filename = ThisFigure.instrument.Source','errcatch');
      if length(filename), mcplot_edit_file(filename); end
    case 3 then // Edit/colormap
      getcolor('Select color within colormap');
    case 4 then // Edit/_preferences
      xsetm();
    case 23 then // View/instrument
      execstr('figname = ThisFigure.instrument.Source','errcatch');
      if length(figname)
        // Strip off the .instr suffix...
        idx=strindex(figname,'.instr');
        if ~isempty(idx)
          idx=idx(length(idx));
          t=str2code(figname);
	  t=t(1:idx-1);
          figname=code2str(t);
        end
        t=figname+'.scg';
        [fid,err]=fileinfo(t);
        if err
	        t = figname+'.out.scg';
                [fid,err]=fileinfo(t);
        end
        if err == 0
          gwin=xget('window');
          nwin = max(winsid())+1;
          xdel(nwin); xbasc(nwin); xset('window',nwin);
          xload(t);
          t = 'Viewing instrument '+t;
          xinfo(t); mprintf('%s\n',t);
          xset("window",gwin);
        end
      end
    case 5 then // Add _colorbar
      mcplot_colorbar();
    case 6 then  // add text
      t = 'Enter the text to add, and click the OK button';
      xinfo(t); mprintf('%s\n',t);
      answer = x_dialog('Text to add to Figure '+string(gwin),'');
      if length(answer)
        t = 'Select the position where to paste the text.';
        xinfo(t); mprintf('%s\n',t);
        [i,x,y] = xclick();
        xstring(x,y,answer);
        xinfo('');
      end
    case 7 then // Save as/_GIF
      mcplot_output('-gif',[],filename+'.gif');
    case 8 then // Save as/EPS (b-w)
      mcplot_output('-ps' ,[],filename+'.eps');
    case 9 then // Save as/EPS (color)
      mcplot_output('-psc',[],filename+'.eps');
    case 10 then // Save as/_Fig (Xfig)
      mcplot_output('-fig',[],filename+'.fig');
    case 11 then // Save as/PPM
      mcplot_output('-ppm',[],filename+'.ppm');
    case 12 then // Save as/Scilab
      mcplot_output('-scg',[],filename+'.scg');
    case 13 then // Colormap/_Jet
      jet();
    case 14 then // Colormap/HSV
      hsv();
    case 15 then // Colormap/hot
      hot();
    case 16 then // Colormap/cool
      cool();
    case 17 then // Colormap/gray
      gray();
    case 18 then // Close all
      mprintf('To Exit Scilab, type ""exit"" at the scilab''s prompt\n');
      exit
    case 20 then // Colormap/pink
      pink();
    case 21 then // Colormap/inverted pink
      rpink();
    case 22 then // about
      t = ['McStas McPlot menu',...
        '', ...
        'This is the McStas McPlot tool menu',...
        'It enables to customize the plot aspects and colors,'...
        'add objects, as well as export in various formats.', ...
        '','Please visit <http://www.neutron.risoe.dk/mcstas>','', ...
        'McStas comes with ABSOLUTELY NO WARRANTY',...
        'This is free software, and you are welcome',...
        'to redistribute it under certain conditions',...
        'as specified in Licence files.'];
      x_message_modeless(t);
    case 24 then // select a window
      w_indexes = [];
      w_titles = '';
      for index=winsid()
        ThisFigure = [];
        execstr('ThisFigure = MCPLOT.Figure_'+string(index),'errcatch');
        if length(ThisFigure)
          w_indexes = [ w_indexes, index ];
          if ~length(w_titles), w_titles  = [ 'Fig '+string(index)+': '+ThisFigure.filename ];
          else w_titles = [ w_titles, 'Fig '+string(index)+': '+ThisFigure.filename ]; end
        end
      end
      if length(w_indexes) > 1
        w_raise = x_choose(w_titles, 'Select a McPlot window');
      elseif length(w_indexes) == 1, w_raise=1;
      else w_raise=0; end
      if w_raise > 0, 
        t='Selecting '+w_titles(w_raise);
        xinfo(t); mprintf('%s\n',t);
        xset('window', w_indexes(w_raise));
      end
    case 25 then
      xclear()
      xtape('replayna', gwin, 90, 0);
    end 
    if item(k)~= 18, xbasr(); end // update plot
endfunction  

function mcplot_edit_file(filename)
// edit a file using either the EDITOR variable, or default editor

  if MSDOS, def_editor = 'notepad'; filesep = '\';
  else def_editor = 'nedit'; filesep = '/'; end
  if length(filename)
      [x,ierr] = fileinfo(filename);
      if ierr, filename = '..'+filesep+filename; end // try one level up
      [x,ierr] = fileinfo(filename);
      if ierr, filename = '..'+filesep+filename; end // try one level up
      [x,ierr] = fileinfo(filename);
      if ~ierr
        t = 'McPlot: Editing file '+filename;
        xinfo(t); mprintf('%s\n',t);
        if ~MSDOS, filename=filename+' &'; end
        unix_g(getenv('EDITOR',def_editor)+' '+filename); 
      end
    end
endfunction

function mcplot_colorbar(zmin,zmax)
// adds a colorbar on the current plot left side 

  if argn(2) == 0 then zmin=0; zmax=10; end
  // get the drawing region size in the active window
  wrect = xgetech();
  // resize current plot and set space for colorbar
  cb_wr = wrect;
  cb_wr(1)=wrect(1)+wrect(3)*0.95;
  cb_wr(3)=wrect(3)*0.05;
  xsetech(cb_wr, [0 zmin 1 zmax ]);
  plotframe([0 zmin 1 zmax ],[1 0 1 0],[%f,%f],['','',''],cb_wr);
  Matplot1((64:-1:1)',[0 zmin 1 zmax ]);
  pl_wr = wrect;
  pl_wr(3)=pl_wr(3)*0.9;
  //plotframe([0 0 1 1 ],[1 0 1 0],[%f,%f],['','',''],pl_wr);
  xsetech(pl_wr, [0 zmin 1 zmax ]);
endfunction // mcplot_colorbar

function mcplot_output(form, win, filename)
// output the current graphic window in the specified format (default is GIF)
// format may be: gif, ps, psc, fig, scilab
  if argn(2) == 0 then form='GIF'; end
  if argn(2) <= 1 then win = -1; end
  if argn(2) <= 2 then filename=''; end
  if length(win) == 0 then win = -1; end
  if win < 0      then win = xget('window'); end
  if length(filename) == 0 then filename='mcstas'; end
  form = convstr(form,"l");
  
  ext = ''; dr = '';
  //    if output is not empty, open driver+xinit(filename)
  if     length(strindex(form,'-ps')),  ext = '.eps';  dr='Pos';
  elseif length(strindex(form,'-psc')), ext = '.eps';  dr='Pos';
  elseif length(strindex(form,'-gif')), ext = '.gif'; dr='GIF';
  elseif length(strindex(form,'-fig')), ext = '.fig'; dr='Fig'; 
  elseif length(strindex(form,'-ppm')), ext = '.ppm'; dr='PPM'; 
  elseif length(strindex(form,'-scg')), ext = '.scg'; dr=''; end
  if length(dr), driver(dr); end
  if length(ext) 
    if ~length(strindex(filename,ext))
      filename=filename+ext;
    end
    if ext ~= '.scg', 
      xinit(filename);
      if dr == 'Pos' & ~length(strindex(form,'psc'))
        gray();
      end
      xtape('replay',win);
      xend(); 
    else xsave(filename);
    end
    if ext == '.eps'
      // Ugly hack for problem with Scilab's BEpsf which 
      // disallows output filenames with more than one dot...
       
      // Last occurance of '/' or '\' - defines Dirname...
      if MSDOS then
        idx_slash=strindex(filename,'\');
      else
        idx_slash=strindex(filename,'/');
      end
      idx_slash=idx_slash(length(idx_slash));
  	    
      filecode=str2code(filename);
      if ~isempty(idx_slash) then
        Dirname=code2str(filecode(1:idx_slash));
      else
	Dirname="";
        idx_slash=0;
      end
      
      // First occurance of '.' after idx_slash
      idx_dot=strindex(filename,'.');
      idx_dot=idx_dot(idx_dot>idx_slash);
      idx_dot=idx_dot(1);
      
      Basename=code2str(filecode(idx_slash+1:idx_dot-1));
      if MSDOS then
        unix_g('""'+SCI+'\bin\BEpsf"" -landscape '+filename);
	unix_g('move '+Dirname+Basename+'.eps '+filename);
      else
        unix_g(SCI+'/bin/BEpsf -landscape '+filename);
	unix_g('mv '+Dirname+Basename+'.eps '+filename);
      end
    end
  end
  driver('Rec');  // default output to screen
  if length(filename) > 0 then 
    t = 'McPlot: Saved image as '+filename+' ('+form+')';
    if ext == '.scg', t = t+'. Load it with xload(...)'; end
    xinfo(t); mprintf('%s\n',t);
  end
endfunction // mcplot_output

// basic routines required by mcplot ------------------------------------------

function mcplot_subplot(m,n,p)
  // from SCI/macros/xdess/subplot.sci (Copyright INRIA)
  [lhs,rhs]=argn(0)
  if rhs==1 then
    p=modulo(m,10)
    n=modulo((m-p)/10,10)
    m=(m-p-10*n)/100
  end
  j=int((p-1)/n)
  i=p-1-n*j

  xsetech([i/n,j/m,1/n,1/m])
endfunction // mcplot_subplot

function d=mcplot_load(d)
// func to load data
// Find first '(' character:
idx=strindex(d.type,'(')+1;
execstr(['S=['+part(d.type,idx:(length(d.type)-1))+'];']);
if ~length(d.data)
 if ~length(strindex(d.format, 'binary'))
  exec(d.filename,-1);p=d.parent;
  if ~execstr('d2='+d.func+'();','errcatch'),d=d2; d.parent=p;end
 else
  if length(strindex(d.format, 'float')), t='f';
  elseif length(strindex(d.format, 'double')), t='d';
  else return; end
  fid=mopen(d.filename, 'rb');
  pS = prod(S);
  x = mget(3*pS, t, fid);
  d.data  =matrix(x(1:pS), S);
  if length(x) >= 3*pS,
   d.errors=matrix(x((pS+1):(2*pS)), S);
   d.events=matrix(x((2*pS+1):(3*pS)), S);
  end
  mclose(fid);
  return
 end
end
endfunction

function d=mcplot_plot(d,p)
  // func to plot data
  if ~length(strindex(d.type,'0d')), d=mcplot_load(d); end
  if ~p, return; end;
  execstr(['l=[',d.xylimits,'];'],'errcatch'); 
  S=size(d.data);
  t1=['['+d.parent+'] '+d.filename+': '+d.title];
  t = [t1;['  '+d.variables+'=['+d.values+']'];['  '+d.signal];['  '+d.statistics]];
  mprintf('%s\n',t(:));
  if length(strindex(d.type,'0d')),return;
  else
    if p==1
      w=winsid();
      if length(w),w=w($)+1; else w=0; end
      xdel(w); xbasc(w); xset('window',w);
    else w = xget('window'); end
    if length(strindex(d.type,'2d'))
      d.x=linspace(l(1),l(2),S(2)); d.y=linspace(l(3),l(4),S(1)); z=d.data;               
      fz=max(abs(z));fx=max(abs(d.x));fy=max(abs(d.y));                                             
      xlab=d.xlabel; ylab=d.ylabel; x=d.x; y=d.y;
      if fx>0,fx=round(log10(fx)); x=x/10^fx; xlab=xlab+' [*10^'+string(fx)+']'; end    
      if fy>0,fy=round(log10(fy)); y=y/10^fy; ylab=ylab+' [*10^'+string(fy)+']'; end    
      if fz>0,fz=round(log10(fz)); z=z/10^fz; t1=t1+' [*10^'+string(fz)+']'; end                    
      jet();
      plot3d1(x,y,z',90,0,xlab+'@'+ylab+'@'+d.zlabel);    
      if p == 2, t = t1; end
      xtitle(t);       
    elseif length(strindex(d.type,'1d'))
      d.x=linspace(l(1),l(2),S(1));
      mcplot_errorbar(d.x,d.data(:,1),d.data(:,2));
      if p == 2, t = t1; end
      xtitle(t,d.xlabel,d.ylabel);
    else
      d.x=linspace(l(1),l(2),max(S));
      plot2d(d.x,d.data);
      if p == 2, t = t1; end
      xtitle(t,d.xlabel,d.ylabel);
    end
  end
  xname(t1);
  
endfunction // mcplot_plot

function mcplot_errorbar(x,y,e)
// function for creating simple errorbar plots...
  // first, estimate plot range
  xmin=min(x);
  xmax=max(x);
  ymin=min(y-e);
  ymax=max(y+e);
//  rect=[xmin xmax ymin ymax];
  plot2d(x,y,rect=[xmin ymin xmax ymax]);
  errbar(x,y,e,e);
endfunction // mcplot_errorbar

function mcplot_set_global(s, gwin, p_in)
  global MCPLOT
  
  // each MCPLOT global field is named 'Figure_'+gwin
  // This structure contains as many 'Subplot_'+p fields of class 'data'
  // and fields of class 'instrument' and 'parameters'
  p = p_in;
  if ~p, p=1; end
  
  if ~length(gwin), gwin = xget('window'); end
  ThisFigure = [];
  // update MCPLOT global variable
  if (type(MCPLOT) == 16 | type(MCPLOT) == 17) & argn(2) > 1
    tag_names = getfield(1,MCPLOT);
    ThisFigure = 'Figure_'+string(gwin);
    if length(find(tag_names == ThisFigure))
      ThisFigure = getfield(ThisFigure,MCPLOT);
    else ThisFigure = '';
    end
  else
    MCPLOT = struct();
    MCPLOT.Figure_0 = 0;
    MCPLOT.MenuInstalled = 0;
    MCPLOT.ShiftedItems = 0;
  end
  if ~length(ThisFigure)
    ThisFigure = struct();
    tag_names = getfield(1,s);
    if length(find(tag_names == 'filename')), t=s.filename;
    elseif length(find(tag_names == 'Source')),t=s.Source;
    elseif length(find(tag_names == 'File')), t=s.File;
    else t='unknown'; end
    ThisFigure.simulation = t;
  end
  if s.class == 'instrument'
    instrument            = struct();
    instrument.Source     = s.Source;
    instrument.name       = s.name;
    instrument.Parameters = s.Parameters;
    instrument.class = 'instrument';
    ThisFigure.instrument = 0;
    ThisFigure.instrument = instrument;
  elseif s.class == 'simulation'
    ThisFigure.simulation            = s.name+'.sci';
  elseif s.class == 'parameters'
    ThisFigure.parameters = 0;
    ThisFigure.parameters = s;
  elseif s.class == 'data'
    if ~p_in, ThisFigure.filename = s.filename; 
    else ThisFigure.filename = ThisFigure.simulation; end
    execstr('ThisFigure.Subplot_'+string(p)+' = 0;');
    execstr('ThisFigure.Subplot_'+string(p)+' = s;');
  end // ignore other classes

  // store Figure McPlot info
  execstr('MCPLOT.Figure_'+string(gwin)+'= 0;');
  execstr('MCPLOT.Figure_'+string(gwin)+'= ThisFigure;');
endfunction

function [data_count, s] = mcplot_scan(s,action, m,n,p, id)
// scans the structure s recursively and opens a graphic window 
// for all found 'data' structures having curves/surfaces data. 
// input: s:      structre where we look for data blocks
//        action: may be 'count', '-plot' and '-overview'
//        m,n,p: indexes for subplot windows
// m may also be used as a string keyword used for searching within McStas
//  structure fields 'filename','title', 'type'
  
  
  if argn(2) == 0 then data_count = 0; return; end
  if type(s) ~= 16 & type(s) ~= 17 then data_count = 0; return; end
  tag_names = getfield(1,s);
  if length(find(tag_names == 'class')) == 0 then 
    if length(find(tag_names == 'data')), s.class = 'data'; 
    else 
      s.class = 'root'; 
      data_count = 0; 
    end
  end
  if argn(2) == 1 then action = ''; end
  if argn(2) == 3 then id = m; end
  if ~length(action), action = '-overview'; end
  w=winsid();
  if length(w),w=w($)+1; else w=0; end
  if length(strindex(action,'-overview')) & argn(2) == 3
    // first count the number of data blocks
    data_count = mcplot_scan(s,'count',id);
    if ~data_count then return; end
    m = floor(sqrt(data_count));
    n = ceil(data_count/m);
    xdel(w); xbasc(w); xset('window',w);
    p = 1;
    mcplot_subplot(m,n,p);
    mcplot_addmenu();
    data_count = 0;
  elseif argn(2) == 3 then 
    m=0; n=0; p=0;
    data_count = p;
  end
  if s.class ~= 'data'
    if s.class == 'parameters' | s.class == 'simulation' | s.class == 'instrument'
      mcplot_set_global(s, w, 0);
    end
    for i=2:max(size(tag_names))   // tag_names(1) is 'struct' (type=17)
      d = getfield(i, s);
      if type(d) == 16 | type(d) == 17 then 
        [ndc, d]   = mcplot_scan(d,action,m,n,data_count,id);
        data_count = ndc;
        if length(d) > 0
          setfield(i, d, s);
        end
      end
    end
  else
    doplot = 1;
    if length(id)
      if ~length(strindex(s.filename,id)) & ~length(strindex(s.title,id)) & ~length(strindex(s.type,id))
        doplot = 0;
      end
    end
    if doplot
      if length(strindex(action,'count')) 
        s = mcplot_plot(s, 0);
      elseif length(strindex(action,'-plot')) 
        s = mcplot_plot(s, 1);
        mcplot_set_global(s, [], 0);
        mcplot_addmenu();
      elseif length(strindex(action,'-overview'))
        mcplot_subplot(m,n,data_count+1);
        s = mcplot_plot(s, 2);
        mcplot_set_global(s, [], data_count+1);
      end
      data_count = data_count+1; 
    end
  end // else class == 'data'
endfunction // mcplot_scan

function [object,count]=mcplot(object, options, id)
// mcplot: plot a McStas simulation result
//
// This function displays a McStas simulation result either as many windows
// or on a single window with subplots. It also returns the McStas simulation 
// structure. An 'id' may be specified for filtering within structure
//
// input:
//  object: one or more simulation name(s) or a single mcstas structure
//  options may contain keywords 
//    '-overview' to plot all results on the same window
//    '-plot'    to plot all results in separate windows
//    'gif'     to export as GIF file(s)
//    'ps'      to export as PostScript file(s)
//    'psc'     to export as color PostScript file(s)
// id is a keyword used for searching within McStas structure fields
//    'filename' and 'title'

// parameter check
if argn(2) == 0, object=''; end
if argn(2) <= 1, options=''; end
if argn(2) <= 2, id = ''; end

if typeof(options) ~= 'string', options = ''; end
if ~length(options), options = '-overview'; end
options = convstr(options, 'l');  // to lower case
if ~length(strindex(options,'plot')) &  ~length(strindex(options,'overview')) &  ~length(strindex(options,'action'))
	options = options+' -overview ';
end

// handle file name specification in 'object'
if typeof(object) == 'string' // if object is a string

  if size(object,2) > 1
    for index=1:size(object,2)
      [object,count] = mcplot(object(index), options, id);
    end
    return
  end
  // if object is a '' string, s = 'mcstas'.
  if length(object) == 0, object = 'mcstas.sci'; end
  [fid, err] = mopen(object, 'r');
  if err ~= 0 // error occured. Calls fileselector (xgetfile)
    object = xgetfile('*.sci', title='Select a McStas simulation file to load');
    if ~length(object), return; end
    [fid, err] = mopen(object, 'r');
    if err ~= 0
      mprintf('%s\n','mcplot: Could not open file '+string(object));
      mprintf('%s\n','mcplot: '+lasterror());
      return
    end
  end
  mclose(fid);
  chdir(dirname(object)); // go into directory where object is
  filename = basename(object);
  if length(strindex(object,'.sci')), filename=filename+'.sci';
  else filename=filename+'.sce'; end
  object=filename;
  //    opens filename with exec(filename,-1)
  exec(object, -1); // compile the file
  mcstas = [];
  execstr('mcstas = get_mcstas();','errcatch');
  if ~length(mcstas)
    // function does not exist. Try using valid name
    valid_name = object;
    to_replace = "!#$%&''""()*+,-.:;<=>?@[\]^`/ ";
    for index=1:length(to_replace)
      if (index), by_char = '_'; else by_char = 'm'; end
      valid_name = strsubst(valid_name, part(to_replace, index), by_char);
    end
    mprintf('mcstas = get_'+valid_name+'();\n')
    execstr('mcstas = get_'+valid_name+'();','errcatch');
    if ~length(mcstas)
      mprintf('mcplot: Could not extract McStas structure from file '+object+'\n');
      mprintf('mcplot: %s\n',lasterror());
      return
    end
  end
  if length(mcstas)
    object = mcstas; mcstas = [];
  end
end

// handles structure loading and ploting
if type(object) ~= 16 & type(object) ~= 17
  mprintf('mcplot: Could not extract the McStas simulation structure.\n')
  return;
else  // if 's' is a 'struct'
  ext = ''; dr = '';
  //    if output is not empty, open driver+xinit(filename)
  if     length(strindex(options,'-ps')),  ext = '.eps';  dr='Pos';
  elseif length(strindex(options,'-psc')), ext = '.eps';  dr='Pos';
  elseif length(strindex(options,'-gif')), ext = '.gif'; dr='GIF';
  elseif length(strindex(options,'-fig')), ext = '.fig'; dr='Fig'; 
  elseif length(strindex(options,'-ppm')), ext = '.ppm'; dr='PPM'; 
  elseif length(strindex(options,'-scg')), ext = '.scg'; dr=''; end
  if length(dr), driver(dr); end
  if length(ext)
    filename = '';
    execstr('filename = object.File;','errcatch');
    if length(filename) == 0 then execstr('filename = object.filename;','errcatch'); end
    if length(filename) == 0 then filename='mcstas'; end
    filename=filename+ext;
    if ~length(strindex(options,'-scg')), xinit(filename); end
    if ~length(strindex(options,'-plot')) & ~length(strindex(options,'-overview'))
      options = options+' overview';
    end
  end
  //  **  send to mcplot_scan(s, options, id)  **
  [count, object] = mcplot_scan(object, options, id);
  //    if output is not empty close xend()
  if length(ext)
    if ext ~= '.scg', xend(); 
    else xsave(filename);
    end
    if ext == '.eps'
      if MSDOS then
        unix_g('""'+SCI+'\bin\BEpsf"" -landscape '+filename);
      else
        unix_g(SCI+'/bin/BEpsf -landscape '+filename);
      end
    end
    t='mcplot: McStas plot exported as file '+filename+' ('+options+')';
    xinfo(t); mprintf('%s\n',t);
  else
    // installs a common menu for all figures
    // in case this could not be done with GTk
    ret = -1;
    while (ret < 0)	// used whith x_choices, or ignore with Tk/GTk
      ret = mcplot_addmenu('common');	
    end
  end
  driver('Rec');  // default output to screen
end

endfunction // mcplot

