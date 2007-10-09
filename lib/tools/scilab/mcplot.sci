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
// Release: McStas 1.10a
// Origin: ILL
//
//  This file is part of the McStas neutron ray-trace simulation package
//  Copyright (C) 1997-2006, All rights reserved
//  Risoe National Laborartory, Roskilde, Denmark
//  Institut Laue Langevin, Grenoble, France
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; version 2 of the License.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

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

  if driver() ~= 'Rec' then return; end
  win = xget('window');
  if argn(2) > 1 & MCPLOT.MenuInstalled, return; end

  // remove McStas menu (if any)
  delmenu(win,'McStas');
  // creates a local McStas menu for looking at data files, and direct exporting
  menu_installed = 0;
  t = [ 'Select a figure', ...
        'Open in a separate window', ...
        'Open in a separate window (Log scale)', ...
        'Edit/_data file', ...
        'Edit/_instrument file', ...
        'Edit/colormap...', ...
        'Edit/preferences...', ...
        'View instrument...', ...
        'Reset View...', ...
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
        'View Scan step...', ...
        'Open McStas result file...', ...
        'About McStas...',...
        'Exit'];        // exit must be last choice
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
    if ~exists('with_tk'), tk_on = 0; else tk_on = with_tk(); end
    if havewindow() & tk_on
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
                  'menu .foo.menu.mcplot.m', ...
                  '.foo.menu.mcplot.m add cascade -label ""Colorbar"" -menu .foo.menu.mcplot.colorbar -underline 0', ...
                  'menu .foo.menu.mcplot.colorbar',...
                  '.foo.menu.mcplot.m add cascade -label ""Edit"" -menu .foo.menu.mcplot.edit -underline 0', ...
                  'menu .foo.menu.mcplot.edit',...
                  '.foo.menu.mcplot.m add cascade -label ""Save"" -menu .foo.menu.mcplot.save -underline 0',...
                  'menu .foo.menu.mcplot.save'];

      for index=1:size(t,2)
        if length(strindex(t(index),'Colormap')), parent='.foo.menu.mcplot.colorbar';
        elseif length(strindex(t(index),'Save')), parent='.foo.menu.mcplot.save';
        elseif length(strindex(t(index),'Edit')), parent='.foo.menu.mcplot.edit';
        else parent = '.foo.menu.mcplot.m'; end
        tcl_script = [ tcl_script, ...
          parent+' add command -label ""'+t(index)+'"" -underline 0  -command {ScilabEval ""mcplot_menu_action('+string(index)+')""}'];

      end
      tcl_script = [ tcl_script, 'pack .foo.menu.mcplot -side left' ];
      // now send it to Tk
      if ~exists('TCL_EvalStr')
        TK_EvalStr(tcl_script);
      else
        TCL_EvalStr(tcl_script);
      end
    elseif argn(2) > 0
      // we shall use x_choices
      rep = x_choices(['Fig '+string(win)+': Choose something to do','Select ""Exit"" or ""Cancel"" to exit McPlot/Scilab'], list(list('Action:', 1, t)));
      if length(rep), mcplot_menu_action(rep); end
      if ~length(rep), mcplot_menu_action(size(t,2)); end
      if rep == size(t,2), win=0; else win = -1; end
    end
  end
endfunction // mcplot_addmenu

function mcplot_menu_action(k, gwin)

  global MCPLOT

  if argn(2) == 1
    gwin = xget('window');
  end

  item = [ 24, 1, 27, 2, 19, 3, 4, 23, 25, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 26, 28, 22, 18];

  if argn(2) == 0, k = 1; end
  if MCPLOT.ShiftedItems
    k = k+1;
  end
  if k <= 0
    mprintf('Oops, the Tcl/Tk uses shifted menu item indexes\n');
    mprintf('This should be corrected now...Retry.\n');
    MCPLOT.ShiftedItems = 1;
    k=1;
  end

  if item(k) == 26 // scan step
    if MCPLOT.ScanWindow < 0
      mprintf('The active window '+string(gwin)+' is not a scan result');
    else
      gwin = MCPLOT.ScanWindow;
    end
  end

  xset('window', gwin); // raise menu activated window

  // extract global data
  ThisFigure = [];
  execstr('ThisFigure = MCPLOT.Figure_'+string(gwin),'errcatch');
  filename = '';
  execstr('filename = ThisFigure.filename','errcatch');

  if ~length(ThisFigure), return; end
  fig_names=getfield(1,ThisFigure);

  select item(k)
    case 0 then
            mprintf('Invalid menu item\n');
    case 1 then // Open in a separate window (duplicate)
      0;  // NOP
    case 2 then // Edit/_data file
      if length(filename), mcplot_edit_file(filename); end
    case 19 then // Edit/instrument file
      execstr('filename = ThisFigure.Source','errcatch');
      if length(filename), mcplot_edit_file(filename); end
    case 3 then // Edit/colormap
      getcolor('Select color within colormap');
    case 4 then // Edit/_preferences
      xsetm();
    case 23 then // View/instrument
      figname = [];
      execstr('figname = ThisFigure.Source','errcatch');
      if length(figname)
        // Strip off the .instr suffix...
        idx=strindex(figname,'.instr');
        if length(idx)
          idx=idx(length(idx));
          t=str2code(figname);
          t=t(1:idx-1);
          figname=code2str(t);
        end
        idx=strindex(figname,'.out');
        if length(idx)
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
        if err
          parameters = [];
          execstr('parameters = ThisFigure.parameters','errcatch');
          if length(parameters)
            // scan parameters structure, excluding 'class','parent','name'
            // The following assumes that the instrument is in current dir...
            if MSDOS
              tmp_parcmd = 'mcdisplay.pl '+figname+'.exe -n 100 -pScilab --save ';
            else
              tmp_parcmd = 'mcdisplay ./'+figname+'.out -n 100 -pScilab --save ';
            end
            tmp_fields = getfield(1,parameters);
            for field=1:size(tmp_fields,2)
              if (tmp_fields(field) ~= 'class' & ...
                tmp_fields(field) ~= 'parent' & ...
                tmp_fields(field) ~= 'name' & ...
                tmp_fields(field) ~= 'dims' & ...
                tmp_fields(field) ~= 'st' & ...
                tmp_fields(field) ~= 'struct')
                tmp_parcmd = tmp_parcmd+' '+tmp_fields(field)+'='+string(getfield(tmp_fields(field), parameters));
              end
            end
            t='Executing:'+tmp_parcmd;
            xinfo(t); mprintf('%s\n',t);
            rep=unix_g(tmp_parcmd);
          end
          t=figname+'.scg';
          [fid,err]=fileinfo(t);
          if err
            t = figname+'.out.scg';
            [fid,err]=fileinfo(t);
          end
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
      mcplot_output('-gif',gwin,filename+'.gif');
    case 8 then // Save as/EPS (b-w)
      mcplot_output('-ps' ,gwin,filename+'.eps');
    case 9 then // Save as/EPS (color)
      mcplot_output('-psc',gwin,filename+'.eps');
    case 10 then // Save as/_Fig (Xfig)
      mcplot_output('-fig',gwin,filename+'.fig');
    case 11 then // Save as/PPM
      mcplot_output('-ppm',gwin,filename+'.ppm');
    case 12 then // Save as/Scilab
      mcplot_output('-scg',gwin,filename+'.scg');
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
      mprintf('To Exit Scilab, use the ""Exit"" item of the McStas menu.\n');
      if exists('McPlotTempFile')
        // Because of Win32's way of backgrounding processes, the perl layer
        // between McStas and mcplot can not be allowed to remove the
        // temporary file (It is gone before Scilab sees it!) Instead,
        // McPlotTempFile stores the filename of the temporary
        // scriptfile, which is then removed on Scilab exit.
        if MSDOS
          unix_g(strcat(['del /q /f ' McPlotTempFile]));
        else // Probably safe to assume unix here?
          unix_g(strcat(['rm -f ' McPlotTempFile]));
        end
      end
      exit
      quit
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
        '','Please visit <http://www.mcstas.org/>','', ...
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
    case 25 then  // reset view
      xclear()
      if MCPLOT.DirectExport ~= 1 then
        num = x_choose(['Top view','X-side view','Y-side view','Default 3D'],['Select view to display']);
        if num==1, xtape('replayna', gwin, 90, 0);
        elseif num==2, xtape('replayna', gwin, 90, 90);
        elseif num==3, xtape('replayna', gwin, 0, 90);
        elseif num==4, xtape('replayna', gwin, -37, 30);
        end
      end
    case 26 then  // select scan step
      pathname = ''; execstr('pathname =ThisFigure.pathname','errcatch');
      filename = ''; execstr('filename =ThisFigure.filename','errcatch');
      source   = ''; execstr('source   =ThisFigure.Source','errcatch');
      ncount   = '?';execstr('ncount   =ThisFigure.Ncount','''errcatch''');
      if length(find(fig_names == 'superdata'))
        scannedvar = ''; execstr('scannedvar=ThisFigure.superdata.scannedvar;','errcatch')
        if length(scannedvar)
          // make a list of scan step items
          index = 0:(ThisFigure.superdata.numpoints-1); index=string(index');
          scannedvar = linspace(ThisFigure.superdata.minvar, ThisFigure.superdata.maxvar, ThisFigure.superdata.numpoints);
          scannedvar = string(scannedvar');
          scannedvar = '[#'+index+'] '+ThisFigure.superdata.scannedvar+'='+scannedvar;
          prompt = [ 'Select a Scan Step to open',...
            '['+source+'] '+pathname+filename, ...
            'Scan of '+ThisFigure.superdata.scannedvar+'='+string(ThisFigure.superdata.minvar)+':'+...
            string(ThisFigure.superdata.maxvar)+' in '+string(ThisFigure.superdata.numpoints)+' points.'];
          selection = x_choose(scannedvar, prompt);
          if selection
            disp('mcplot('''+pathname+string(selection-1)+''',''-overview'');');
            mcplot(pathname+string(selection-1),'-overview');
          end
        end
      end // if length(find(fig_names == 'superdata'))
    case 27 then  // duplicate (log scale)
      0; // NOP
    case 28 then  // open new mcstas.m file
      // open a new window
      eval('mcplot('','')');
    end
    if item(k)~= 18, xbasr(); end // update plot

    // handle duplicate linear/log cases
    if item(k)==27 | item(k) == 1
      t='Figure '+string(gwin)+' (active): Click on the plot to duplicate/enlarge.';
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
      if item(k)==27 then t = t+' (Log scale)'; end
      // extract global data
      d = [];
      execstr('d = ThisFigure.Subplot_'+string(p),'errcatch');
      if length(d)
        dat_names = getfield(1,d);
        if ~length(find(dat_names == 'Source')) & length(find(fig_names == 'Source')), d.Source = ThisFigure.Source; end
        if ~length(find(dat_names == 'Ncount')) & length(find(fig_names == 'Ncount')), d.Ncount = ThisFigure.Ncount; end
        if ~length(find(dat_names == 'parameters')) & length(find(fig_names == 'parameters')), d.parameters=0; d.parameters = ThisFigure.parameters; end
        if ~length(find(dat_names == 'pathname')) & length(find(fig_names == 'pathname')), d.pathname = ThisFigure.pathname; end
        // set Log scale if required
        if item(k)==27 then execstr('MCPLOT.LogScale = 1;','errcatch'); end
        mcplot_scan(d,'-plot','');
        // unset Log scale if required
        if item(k)==27 then execstr('MCPLOT.LogScale = 0;','errcatch'); end
        nwin = xget('window'); // new opened window
        // update the MCPLOT global variable
        execstr('MCPLOT.Figure_'+string(nwin)+'.filename = ThisFigure.filename;','errcatch');
        mcplot_set_global(d, nwin, 0);
        xclear();
        if MCPLOT.DirectExport ~= 1
          xtape('replayna', nwin, 90, 0);        // top view
        end
        mcplot_fig_legend(d,'','');
      end
      xinfo(t); mprintf('%s\n',t);
    end
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
  if argn(2) == 0 then form='GIF'; end
  if argn(2) <= 1 then win = -1; end
  if argn(2) <= 2 then filename=''; end
  if length(win) == 0 then win = -1; end

  filename = mcplot_output_begin(form, filename);
  mcplot_output_end(form, win, filename);
  driver('Rec');  // default output to screen
endfunction // mcplot_output

function filename = mcplot_output_begin(form, filename)
// initiate output in the specified format (default is GIF)
// format may be: gif, ps, psc, fig, scilab
  if argn(2) == 0 then form='GIF'; end
  if argn(2) <= 1 then filename=''; end
  if length(filename) == 0 then filename='mcstas'; end
  form = convstr(form,"l");

  ext = ''; dr = '';
  //    if output is not empty, open driver+xinit(filename)
  if     length(strindex(form,'-ps')),  ext = '.eps';  dr='Pos';
  elseif length(strindex(form,'-psc')), ext = '.eps';  dr='Pos';
  elseif length(strindex(form,'-gif')), ext = '.gif';  dr='GIF';
  elseif length(strindex(form,'-fig')), ext = '.fig';  dr='Fig';
  elseif length(strindex(form,'-ppm')), ext = '.ppm';  dr='PPM';
  elseif length(strindex(form,'Rec')),  ext = '';      dr='Rec';
  elseif length(strindex(form,'-scg')), ext = '.scg';  dr=''; end
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
    end
  end
endfunction // mcplot_output_begin

function mcplot_output_end(form, win, filename)
// output the current graphic window in the specified format (default is GIF)
// format may be: gif, ps, psc, fig, scilab
  if argn(2) == 0 then form='GIF'; end
  if argn(2) <= 1 then win = -1; end
  if argn(2) <= 2 then filename=''; end
  if length(win) == 0 then win = -1; end
  if win < 0      then win = xget('window'); end
  ThisFigure = [];
  execstr('ThisFigure = MCPLOT.Figure_'+string(win),'errcatch');
  filename = '';
  execstr('filename = ThisFigure.filename','errcatch');
  if length(filename) == 0 then filename='mcstas'; end
  form = convstr(form,"l");

  ext = ''; dr = '';
  //    if output is not empty
  if     length(strindex(form,'-ps')),  ext = '.eps';
  elseif length(strindex(form,'-psc')), ext = '.eps';
  elseif length(strindex(form,'-gif')), ext = '.gif';
  elseif length(strindex(form,'-fig')), ext = '.fig';
  elseif length(strindex(form,'-ppm')), ext = '.ppm';
  elseif length(strindex(form,'-scg')), ext = '.scg';  end
  if length(ext)
    if ~length(strindex(filename,ext))
      filename=filename+ext;
    end
    if ext ~= '.scg',
      if MCPLOT.DirectExport ~= 1, xtape('replay',win); end
      xend();
    else xsave(filename);
    end
    if ext == '.eps'
      [Dirname, Basename, Ext]= mcplot_fileparts(filename);

      if MSDOS then
        unix_g('""'+SCI+'\bin\BEpsf"" -landscape '+filename);
        unix_g('move '+Dirname+Basename+'.eps '+filename);
      else
        unix_g(SCI+'/bin/BEpsf -landscape '+filename);
        unix_g('mv '+Dirname+Basename+'.eps '+filename);
      end
    end
  end
  if length(filename) > 0 & length(ext) > 0 then
    t = 'McPlot: Saved image Fig '+string(win)+' as '+filename+' ('+form+')';
    if ext == '.scg', t = t+'. Load it with scilab> xload(""'+filename+'"")'; end
    xinfo(t); mprintf('%s\n',t);
  end
endfunction // mcplot_output_end

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

  xsetech([i/n,j/m*0.95,1/n,1/m*0.95])
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
 end
 if length(strindex(d.type,'1d'))
  if size(d.data,2) > 1 & size(d.errors,2) ==0
   d.errors = d.data(:,2);
   d.data   = d.data(:,1);
  end
 end
end
endfunction

function d=mcplot_plot(d,p)

  global MCPLOT

  // func to plot data
  if ~length(strindex(d.type,'0d')), d=mcplot_load(d); end
  if ~length(d.values), d.values = string(sum(sum(d.data)))+' '+string(sum(sum(d.errors))); end
  if ~length(d.signal), d.signal = 'Min='+string(min(min(d.data)))+';+Max='+string(max(max(d.data)))+';+Mean='+string(mean(mean(d.data))); end
  if ~p, return; end;
  execstr(['l=[',d.xylimits,'];'],'errcatch');
  S=size(d.data);
  logscale = 0;
  execstr('logscale=MCPLOT.LogScale','errcatch');
  t1=['['+d.parent+'] '+d.filename];
  if logscale then t1=t1+' (Log)'; end
  t2 = t1+': '+d.title;
  t = [t2;['  '+d.variables+'=['+d.values+']'];['  '+d.signal];['  '+d.statistics]];
  mprintf('%s\n',t(:));
  if length(strindex(d.type,'0d')),return;
  else
    if p==1
      w=winsid();
      if length(w),w=w($)+1; else w=0; end
      xdel(w); xbasc(w); xset('window',w);
    end
    if length(strindex(d.type,'2d'))
      if S(2) > 1, d.stepx=abs(l(1)-l(2))/(S(2)-1); else d.stepx=0; end
      if S(1) > 1, d.stepy=abs(l(3)-l(4))/(S(1)-1); else d.stepy=0; end
      d.x=linspace(l(1)+d.stepx/2,l(2)-d.stepx/2,S(2));
      d.y=linspace(l(3)+d.stepy/2,l(4)-d.stepy/2,S(1)); z=d.data;
      if logscale == 1 then
        minz=1e-10;
        index_p=find(z>0); index_n=find(z<=0);
        if length(index_p) then
          minz=min(z(index_p)); z(index_p) = log10(z(index_p));
        end
        if length(index_n) then
          z(index_n) = log10(minz/10);
        end
      end
      fz=max(abs(z));fx=max(abs(d.x));fy=max(abs(d.y));
      xlab=d.xlabel; ylab=d.ylabel; x=d.x; y=d.y;

      if fx>0,fx=round(log10(fx)); x=x/10^fx; xlab=xlab+' [*10^'+string(fx)+']'; end
      if fy>0,fy=round(log10(fy)); y=y/10^fy; ylab=ylab+' [*10^'+string(fy)+']'; end
      if fz>0,fz=round(log10(fz)); z=z/10^fz; t1=t1+' [*10^'+string(fz)+']'; end
      jet();
      plot3d1(x,y,z',90,0,xlab+'@'+ylab+'@'+d.zlabel,[-1,2,4]);
      if p == 2, t = d.filename; end
      xtitle(t);
    elseif length(strindex(d.type,'1d')) & length(find(getfield(1,d) == 'errors'))
      z = d.data; e = d.errors;
      if logscale == 1 then
        minz=0;
        index_p=find(z>0); index_n=find(z<=0);
        if length(index_p) then
          minz=min(z(index_p)); e(index_p)=e(index_p)./z(index_p);
          z(index_p) = log10(z(index_p));
        end
        if length(index_n) then
          z(index_n) = log10(minz/10);
          e(index_n) = 0;
        end
      end
      if max(S) > 1, d.stepx=abs(l(1)-l(2))/(max(S)-1);
      else d.stepx=0; end
      d.x=linspace(l(1)+d.stepx/2,l(2)-d.stepx/2,max(S));
      mcplot_errorbar(d.x,z,e);
      if p == 2, t = d.filename; end
      xtitle(t,d.xlabel,d.ylabel);
    else
      z = d.data;
      if logscale == 1 then
        minz=0;
        index_p=find(z>0); index_n=find(z<=0);
        if length(index_p) then
          minz=min(z(index_p)); z(index_p) = log10(z(index_p));
        end
        if length(index_n) then
          z(index_n) = log10(minz/10);
        end
      end
      if max(S) > 1, d.stepx=abs(l(1)-l(2))/(max(S)-1);
      else d.stepx=0; end
      d.x=linspace(l(1)-d.stepx/2,l(2)+d.stepx/2,max(S));
      plot2d(d.x,z);
      if p == 2, t = d.filename; end
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
  plot2d(x,y,rect=[xmin ymin xmax ymax]);
  errbar(x,y,e,e);
endfunction // mcplot_errorbar

function mcplot_set_global(s, gwin, p_in)
  global MCPLOT

  // each MCPLOT global field is named 'Figure_'+gwin
  // This structure contains as many 'Subplot_'+p fields of class 'data'
  // and fields of class 'instrument' and 'parameters'
  if length(p_in) == 3
    m = p_in(1);
    n = p_in(2);
    p = p_in(3);
    p_in = p;
  else m=0; n=0; p=p_in; end
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
  elseif ~length(s)
      MCPLOT = struct();
      MCPLOT.Figure_0 = 0;
      MCPLOT.MenuInstalled = 0;
      MCPLOT.DirectExport  = 0;
      MCPLOT.ShiftedItems = 0;
      MCPLOT.LogScale = 0;
      MCPLOT.ScanWindow = -1;
      return
  end
  if gwin == -1 then return; end
  tag_names = getfield(1,s);
  if ~length(ThisFigure)
    ThisFigure = struct();
    if length(find(tag_names == 'filename')), t=s.filename;
    elseif length(find(tag_names == 'Source')),t=s.Source;
    elseif length(find(tag_names == 'File')), t=s.File;
    else t='unknown'; end
    ThisFigure.filename = t;
    ThisFigure.overview = [];
  end
  if length(find(tag_names == 'Source')), ThisFigure.Source=s.Source; end
  if length(find(tag_names == 'Date')), ThisFigure.Date=s.Date; end
  if length(find(tag_names == 'Ncount')), ThisFigure.Ncount=s.Ncount; end
  if length(find(tag_names == 'parameters')), ThisFigure.parameters=s.parameters; end
  if s.class == 'instrument'
    instrument            = struct();
    instrument.Source     = s.Source;
    instrument.name       = s.name;
    instrument.Parameters = s.Parameters;
    instrument.class = 'instrument';
    ThisFigure.instrument = 0;
    ThisFigure.instrument = instrument;
  elseif s.class == 'simulation'
    [a,b,ext] = mcplot_fileparts(s.name);
    if ext == '.m', ThisFigure.filename = s.name+'.m';
    else ThisFigure.filename = s.name; end
  elseif s.class == 'parameters'
    ThisFigure.parameters = 0;
    ThisFigure.parameters = s;
  elseif s.class == 'data'
    fig_names = getfield(1,ThisFigure);
    if ~p_in, ThisFigure.filename = s.filename; end
    if length(find(tag_names == 'ratio')) & ~length(find(tag_names == 'Ncount'))
      ThisFigure.Ncount = s.ratio; end
    execstr('ThisFigure.Subplot_'+string(p)+' = 0;');
    execstr('ThisFigure.Subplot_'+string(p)+' = s;');
  elseif s.class == 'superdata'
    ThisFigure.superdata = 0;
    ThisFigure.superdata = s;
    if MCPLOT.ScanWindow < 0
      MCPLOT.ScanWindow = gwin;
    end
  end // ignore other classes

  if m*n, ThisFigure.overview = [m n p]; end

  // store Figure McPlot info
  execstr('MCPLOT.Figure_'+string(gwin)+'= 0;');
  execstr('MCPLOT.Figure_'+string(gwin)+'= ThisFigure;');
endfunction // mcplot_set_global

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
    if driver() == 'Rec' then xdel(w); end
    xbasc(w);
    if driver() == 'Rec' then xset('window',w); end
    p = 1;
    mcplot_subplot(m,n,p);
    if driver() == 'Rec' then mcplot_addmenu(); end
    data_count = 0;
  elseif argn(2) == 3 then
    m=0; n=0; p=0;
    data_count = p;
  else data_count = p;
  end
  if s.class ~= 'data'
    if s.class == 'parameters' | s.class == 'simulation' | ...
       s.class == 'instrument' | s.class == 'superdata'
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
        mcplot_set_global(s, [], [m,n,data_count+1]);
      end
      data_count = data_count+1;
    end
  end // else class == 'data'
endfunction // mcplot_scan

function [Dirname, Basename, Ext] = mcplot_fileparts(filename)
// mcplot_fileparts: separate a full file/path name into bits

  // Last occurance of '/' or '\' - defines Dirname...
  if MSDOS then filesep = '\'; else filesep = '/'; end

  idx_slash=strindex(filename,filesep);
  idx_slash=idx_slash(length(idx_slash));

  filecode=str2code(filename(1));
  if length(idx_slash) then
    Dirname=part(filename,(1:idx_slash));
  else
    Dirname="";
    idx_slash=0;
  end

  // First occurance of '.' after idx_slash
  idx_dot = length(filecode)+1;
  Ext="";
  idx_dot=strindex(filename,'.');
  if length(idx_dot)
    idx_dot=idx_dot(find(idx_dot>idx_slash));
    if length(idx_dot)
      idx_dot=idx_dot(1);
      Ext = part(filename,(idx_dot:length(filecode)));
    end
  end
  Basename=part(filename,((idx_slash+1):(idx_dot-1)));

endfunction // mcplot_fileparts

function mcplot_fig_legend(object,filename, pathname)
// add legend, update object and global data
  // extract global data
  global MCPLOT

  gwin = xget('window');
  fig_names = getfield(1,MCPLOT);
  ThisFigure = 'Figure_'+string(gwin);
  if length(find(fig_names == ThisFigure))
    ThisFigure = getfield(ThisFigure,MCPLOT);
  else ThisFigure = '';
  end

  fig_names=getfield(1,ThisFigure);
  obj_names=getfield(1,object);

  source   = ''; execstr('source=ThisFigure.instrument.Source','errcatch');
  if ~length(source), execstr('source=ThisFigure.Source','errcatch'); end
  if ~length(source), source='McStas'; end
  sdate    = ''; execstr('sdate=ThisFigure.Date','errcatch');
  if ~length(sdate), sdate='unknown'; end
  if type(sdate) ~= 10,
    sdate = getdate(sdate);
    sdate = string(sdate(6))+'/'+string(sdate(2))+'/'+string(sdate(1))+' - '+string(sdate(7))+':'+string(sdate(8))+':'+string(sdate(9));
  end
  ncount   = ''; execstr('ncount   =ThisFigure.Ncount','errcatch');
  overview = ''; execstr('overview =ThisFigure.overview','errcatch');
  if ~length(filename) & length(find(obj_names=='filename')), filename=object.filename; end
  if ~length(filename) & length(find(fig_names=='filename')),  filename=ThisFigure.filename; end
  if ~length(pathname) & length(find(obj_names=='pathname')), pathname=object.pathname; end
  ThisFigure.pathname = pathname;
  execstr('MCPLOT.Figure_'+string(gwin)+'= ThisFigure;');
  // setup the overview title
  t1 = '['+source+'] '+pathname+filename;
  if length(overview), xname(t1); end
  t2='Ncount:'+ncount+'; Date: '+sdate;
  scannedvar=''; execstr('scannedvar=ThisFigure.superdata.scannedvar','errcatch');
  if length(scannedvar)
    t2 = t2+'. Scan of '+ThisFigure.superdata.scannedvar+'='+string(ThisFigure.superdata.minvar)+':'+ ...
      string(ThisFigure.superdata.maxvar)+' in '+string(ThisFigure.superdata.numpoints)+' points.';
  end
  parameters = ''; execstr('parameters =ThisFigure.parameters','errcatch');
  if length(parameters)
    // scan parameters structure, excluding 'class','parent','name'
    t3 = '';
    tmp_fields = getfield(1,parameters);
    for field=1:size(tmp_fields,2)
      if (tmp_fields(field) ~= 'class' & ...
        tmp_fields(field) ~= 'parent' & ...
        tmp_fields(field) ~= 'name' & ...
        tmp_fields(field) ~= 'dims' & ...
        tmp_fields(field) ~= 'st' & ...
        tmp_fields(field) ~= 'struct')
        t3 = t3+' '+tmp_fields(field)+'='+string(getfield(tmp_fields(field), parameters));
      end
    end
  else
    t3 = 'unknown parameters';
  end

  [wrect, frect, logflag, arect] = xgetech();
  xsetech([0,0,1,1],[0,0,1,1]);
  xsetech(arect=[0.05,0.05,0.05,0.05]);
  xstring(0,0,[t1;t2;t3],0,1);
  xsetech(wrect, frect);
  xsetech(arect=arect);

endfunction // mcplot_fig_legend

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
//    '-gif'     to export as GIF file(s)
//    '-ps'      to export as PostScript file(s)
//    '-psc'     to export as color PostScript file(s)
// id is a keyword used for searching within McStas structure fields
//    'filename' and 'title'

// parameter check
global MCPLOT

if argn(2) == 0, object=''; end
if argn(2) <= 1, options=''; end
if argn(2) <= 2, id = ''; end

mcplot_set_global([], -1, []); // set up global MCPLOT

if typeof(options) ~= 'string', options = ''; end
if ~length(options), options = '-overview'; end
options = convstr(options, 'l');  // to lower case
if ~length(strindex(options,'plot')) &  ~length(strindex(options,'overview')) &  ~length(strindex(options,'action'))
        options = options+' -overview ';
end

pathname = '';
filename = '';

if MSDOS, filesep = '\';
else filesep = '/'; end

form = 'Rec';
//    if output is not empty, open driver+xinit(filename)
if     length(strindex(options,'-ps')),  form = '-ps' ;
elseif length(strindex(options,'-psc')), form = '-psc';
elseif length(strindex(options,'-gif')), form = '-gif';
elseif length(strindex(options,'-fig')), form = '-fig';
elseif length(strindex(options,'-ppm')), form = '-ppm';
elseif length(strindex(options,'-scg')), form = '-scg'; end

if form ~= 'Rec' then MCPLOT.DirectExport = 1; end

// handle file name specification in 'object'
if typeof(object) == 'string' // if object is a string

  if size(object,2) > 1
    for index=1:size(object,2)
      [object,count] = mcplot(object(index), options, id);
    end
    return
  end
  // if object is a '' string, s = 'mcstas'.
  if ~length(object), object = 'mcstas.sci'; end
  // checks for directories
  cur_dir = pwd();
  object_orig = object;
  is_dir = 1; execstr('is_dir=chdir(object)','errcatch');
  if is_dir == 0  // OK
    object = object_orig+filesep+'mcstas.sci';
    chdir(cur_dir);
  else
    object = object_orig;
  end
  [fid, err] = mopen(object, 'r');
  if err ~= 0 // error occured. Calls fileselector (xgetfile)
    if ~length(form) | form == 'Rec'
      object = xgetfile('*.sci', title='Select a McStas/Scilab simulation file to load');
    else
      mprintf('%s\n','mcplot: Could not open file '+string(object)+' for auto export');
      return;
    end
    if ~length(object), return; end
    [fid, err] = mopen(object, 'r');
    if err ~= 0
      mprintf('%s\n','mcplot: Could not open file '+string(object));
      mprintf('%s\n','mcplot: '+lasterror());
      return
    end
  end
  mclose(fid);
  object_orig = object;
  mprintf('Opening file '+string(object)+'\n');
  [pathname, object, ext]= mcplot_fileparts(object);
  filename = object+ext;
  object = filename;

  if length(pathname),
    cur_dir = pwd();
    chdir(pathname);
    if part(pathname,length(pathname)) ~= filesep, pathname=pathname+filesep; end
  end
  //    opens filename with exec(filename,-1)
  exec(filename, -1); // compile the file
  mcstas = [];
  mprintf('mcstas = get_mcstas();\n');
  execstr('mcstas = get_mcstas();','errcatch');
  if ~length(mcstas)
    mprintf('mcstas = get_mcstas_'+part(ext,[2:length(ext)])+'();\n');
    execstr('mcstas = get_mcstas_'+part(ext,[2:length(ext)])+'();','errcatch');
  end
  if ~length(mcstas)
    // function does not exist. Try using valid name
    valid_name = object;
    to_replace = "!#$%&''""()*+,-.:;<=>?@[\]^`/ ";
    for index=1:length(to_replace)
      if (index), by_char = '_'; else by_char = 'm'; end
      valid_name = strsubst(valid_name, part(to_replace, index), by_char);
    end
    mprintf('mcstas = get_'+valid_name+'();\n');
    execstr('mcstas = get_'+valid_name+'();','errcatch');
    if ~length(mcstas)
      mprintf('mcplot: Could not extract McStas structure from file '+object+'\n');
      mprintf('        This is not a Scilab script (other format or binary ?)');
      mprintf('mcplot: %s\n',lasterror());
      if length(pathname), chdir(cur_dir); end
      return
    end
  end
  if length(mcstas)
    object = mcstas; mcstas = [];
  end
  if length(pathname), chdir(cur_dir); end
end

// handles structure loading and ploting
if type(object) ~= 16 & type(object) ~= 17
  mprintf('mcplot: Could not extract the McStas simulation structure.\n');
  return;
else  // if 's' is a 'struct'
  if ~length(strindex(getversion(),'2.6')) & ~length(strindex(getversion(),'2.7'))
    set("old_style","on");
  end
  if MCPLOT.DirectExport == 1
    filename = '';
    execstr('filename = object.File;','errcatch');
    if length(filename) == 0 then execstr('filename = object.filename;','errcatch'); end
    if length(filename) == 0 then filename='mcstas'; end
    mcplot_output_begin(form,filename);
    if ~length(strindex(options,'-plot')) & ~length(strindex(options,'-overview'))
      options = options+' overview';
    end
  else
    mcplot_output_begin(form,filename);
  end

  //  **  send to mcplot_scan(s, options, id)  *
  [count, object] = mcplot_scan(object, options, id);
  mcplot_fig_legend(object,filename, pathname);

  // if output is not empty do not install menu
  if MCPLOT.DirectExport == 0
    // installs a common menu for all figures
    // in case this could not be done with GTk/Tk
    ret = -1;
    while (ret < 0)        // used whith x_choices, or ignore with Tk/GTk
      ret = mcplot_addmenu('common');
    end
  end
  mcplot_output_end(form, -1, filename);
end

endfunction // mcplot

