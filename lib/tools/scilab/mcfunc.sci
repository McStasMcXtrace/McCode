// script for definition of colormaps and usefull functions for McStas

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
  h=hotcolormap(64);
  xset('colormap',1-h);
endfunction

function g=gray()
  g=graycolormap(64);
  xset('colormap',g);
endfunction

function win = mcplot_getwin(win, name, m, n)
// if no argument is given, searches for the first non used window 
// initialize (open and clear) the given or found window
  if argn(2) == 0 then win = []; end
  if argn(2) < 2  then name = ''; end
  if length(name) == 0 then name = 'mcstas'; end
  if length(win) == 0 
    win = 0;
    used_wins = winsid();
    if length(used_wins) > 0
      while (length(find(win == used_wins)) == 1)
        win = win+1;
      end
    end
  end
  xbasc(win);
  xset('window',win);
  // remove McStas menu (if any)
  delmenu(win,'McStas')
  // creates a local McStas menu for looking at data files, and direct exporting
  addmenu(win, 'mcplot', ...
    [ 'Open in a separate window', ...      //  'mcplot_scan(s, 'plot',m,n,p); p obtained from xclick() on m,n subplot grid
      'Edit data file '+name, ...           //  ['edit '+name]
      'Save as '+name+'.gif', ...           //  ['mcplot_output(''gif'','+string(win)+','+name+');'],
      'Save as '+name+'.eps (b/w)', ...     //  ['mcplot_output(''ps'','+string(win)+','+name+');'], .
      'Save as '+name+'.eps (color)', ...   //  ['mcplot_output(''psc'','+string(win)+','+name+');'],
      'Save as '+name+'.fig (Xfig)', ...    //  ['mcplot_output(''fig'','+string(win)+','+name+');'],
      'Save as '+name+'.scg (Scilab)', ...  //  ['mcplot_output(''scilab'','+string(win)+','+name+');'
      'Edit colormap...', ...               //  'getcolor(''Current colormap'');', ...
      'Edit preferences...', ...            //  'xsetm()'
      'Colormap: Jet', ...                  //  'jet(); xbasr();', ...
      'Colormap: HSV', ...                  //  'hsv(); xbasr();', ...
      'Colormap: Hot (red)', ...            //  'hot(); xbasr();', ...
      'Colormap: Cool (blue)', ...          //  'cool(); xbasr();', ...
      'Colormap: Gray'], ...                //  'gray(); xbasr();']);
      list(0,'mcplot_menu_action)');
      
endfunction // mcplot_getwin

function mcplot_menu_action(name, entry, win)
endfunction // mcplot_menu_action

function d_out = mcplot_data(d, win)
// plots a McStas data block as 2d plot or 3d plot in the active window
// Also displays integrated values for all data blocks
  d_out = [];
  if argn(2) == 0 then return; end
  if typeof(d) ~= 'struct' then return; end
  tag_names = getfield(1,d);
  if length(strindex('class', tag_names)) == 0 then return; end
  if d.class ~= 'data' then return; end
  S=size(d.data);
  if max(S) == 0 & length(d.filename) > 0
    // opens associated file and reads data from it
    exec(d.filename,-1);  // loads function to get the data
    // calls loaded function and overload data structure
    parent = d.parent;
    ierr = execstr('d='+d.func+'();','errcatch');
    if ierr == 0
      d.parent = parent;
      d_out    = d;
      tag_names = getfield(1,d);
    else 
      d.title = d.title+' [no plot]'
    end
  end
  S=size(d.data);
  if max(S)
    if length(strindex('xlimits', tag_names)) > 0
      l=d.xlimits+' 0 0'; 
      is1d = 1;
    else 
      is1d = 0;
      l=d.xylimits; 
    end
    execstr('l=['+l+']'); 
    x=linspace(l(1),l(2),S(1));
    y=linspace(l(3),l(4),S(2));
    if is1d & size(d.data,2) >= 2
      data = d.data(:,2);
      err  = d.data(:,3);
    else
      data = d.data; err=[];
    end
    f=round(log10(max(data))); 
    fx=max(abs(x)); fy = max(abs(y));
    if fx > 0 then fx=log10(fx); else fx = []; end
    if fy > 0 then fy=log10(fy); else fy = []; end
    if length(fx) 
      if length(fy) then f = f-round((fx+fy)/2);
      else f = f-round(fx); end
    end
    data=data/10^f; err=err/10^f;
    if argn(2) == 2
      mcplot_getwin(win, d.filename);
    end
    if is1d
      if length(err) > 0 then errbar(x, data, err/2, err/2);
      else plot2d(x,data); end
      xgrid(3);
    else 
    // add colorbar for 3d when wrect=xgetech is big enough
      jet();
      wrect=xgetech();
      if wrect(4)*wrect(3) > 0.25 then
        mcplot_colorbar(min(data),max(data));
      end
      plot3d1(x,y,data); 
    end 
    t = ['['+d.parent+'] '+d.filename+': '+d.title+' [*10^'+string(f)+']';...
        d.variables+'=['+d.values+']';...
        d.signal;d.statistics];
    xtitle(t,d.xlabel,d.ylabel);
    xname('['+d.parent+'] '+d.filename+': '+d.title);
    xinfo(d.variables+'=['+d.values+']');
  end
  mprintf('Data ['+d.parent+'] '+d.filename+': '+d.title+'\n')
  mprintf('  '+d.variables+'=['+d.values+']\n');
  
endfunction // mcplot_data

function [data_count, s] = mcplot_scan(s,action, m,n,p)
// scans the structure s recursively and opens a graphic window 
// for all found 'data' structures having curves/surfaces data. 
// input: s:      structre where we look for data blocks
//        action: may be 'count', 'plot' and 'overview'
//        m,n,p: indexes for subplot windows
  
  if argn(2) == 0 then data_count = 0; return; end
  if typeof(s) ~= 'struct' then data_count = 0; return; end
  tag_names = getfield(1,s);
  if length(strindex('class', tag_names)) == 0 then 
    s.class = 'root'; 
    data_count = 0;
  end
  if argn(2) == 1 then action = 'overview'; end
  
  if length(strindex('overview',action)) &  argn(2) <= 2
    // first count the number of data blocks
    data_count = mcplot_scan(s,'count');
    if ~data_count then return; end
    m = floor(sqrt(data_count));
    n = ceil(data_count/m);
    p = mcplot_getwin([], s.filename, m, n);
    xbasc(p);
    xset('window',p);
    p = 1;
    subplot(m,n,p);
    data_count = 0;
  elseif argn(2) == 2 then 
    m=0; n=0; p=0;
    data_count = p;
  end
  
  if s.class ~= 'data'
    for i=2:size(tag_names,1)   // tag_names(1) is 'struct'
      d = getfield(i, s);
      if typeof(d) == 'struct' then 
        [ndc, d]   = mcplot_scan(d,action,m,n,data_count);
        data_count = data_count + ndc;
        if length(d) > 0
          setfield(i, d, s);
        end
      end
    end
  else
    if length(strindex('count',action)) then 
      data_count = 1; 
      return
    end
    if length(strindex('plot',action)) 
      mcplot_getwin([], s.filename, m, n, p);
      s = mcplot_data(s);
    elseif length(strindex('overview',action))
      subplot(m,n,data_count+1);
      s = mcplot_data(s);
    end
    data_count = 1;
  end
endfunction // mcplot_scan

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
  plotframe([0 zmin 1 zmax ],[1 0 2 4],[%f,%f],['[z]','',''],cb_wr);
  Matplot1((64:-1:1)',[0 zmin 1 zmax ]);
  pl_wr = wrect;
  pl_wr(3)=pl_wr(3)*0.9;
  //plotframe([0 0 1 1 ],[1 0 1 0],[%f,%f],['','',''],pl_wr);
  xsetech(pl_wr, [0 zmin 1 zmax ]);
endfunction

function mcplot_output(form, win, filename)
// output the current graphic window in the specified format (default is GIF)
// format may be: gif, ps, psc, fig, scilab
  if argn(2) == 0 then form='GIF'; end
  if argn(2) <= 1 then win = -1; end
  if argn(2) <= 2 then filename=''; end
  if length(win) == 0 then win = -1; end
  if win < 0      then win = xget('window'); end
  if length(filename) == 0 then filename='mcstas'; end
  if length(strindex('.', filename)) == 0 then add_ext=1; else add_ext=0; end
  form = convstr(form,"l");
  f = '';
  if length(strindex('gif',form)) 
    driver('GIF')
    if add_ext then f=filename+'.gif'; else f=filename; end
    xinit(f);
    xtape('replay',win);
    xend();
    driver('Rec');
  end
  if length(strindex('psc',form)) 
    driver('Pos')
    if add_ext then f=filename+'.ps'; else f=filename; end
    xinit(f);
    xtape('replay',win);
    xend();
    driver('Rec');
    if MSDOS then
      unix_g('""'+SCI+'\bin\BEpsf"" -landscape '+f);
    else
      unix_g(SCI+'/bin/BEpsf -landscape '+f);
    end
  end
  if length(strindex('ps',form)) 
    c = xget('colormap');
    driver('Pos')
    if add_ext then f=filename+'.ps'; else f=filename; end
    xinit(f);
    gray();
    xtape('replay',win);
    xend();
    driver('Rec');
    xset('colormap',c);
    if MSDOS then
      unix_g('""'+SCI+'\bin\BEpsf"" -landscape '+f);
    else
      unix_g(SCI+'/bin/BEpsf -landscape '+f);
    end
  end
  if length(strindex('fig',form)) 
    driver('Fig')
    if add_ext then f=filename+'.fig'; else f=filename; end
    xinit(f);
    xtape('replay',win);
    xend();
    driver('Rec');
  end
  if length(strindex('scilab',form)) 
    if add_ext then f=filename+'.scg'; else f=filename; end
    xsave(f);
  end
  if length(f) > 0 then disp('Saved image as '+f+' ('+form+')'); end
endfunction // mcplot_output

function mcplot(s,plot_type, output)
// s: structure, or data file name, or default (mcstas.sci)

// plot_type: 'plot' 'overview' 
// output:    gif, ps, psc, fig, scilab
//
// if s is a '' string, s = 'mcstas'.
// if s is a string
//    opens filename with exec(filename,-1)
//    if does not exist, open fileselector (xgetfile)
//    call mcplot(s,plot_type, output)
//    return
// if 's' is a 'struct'
//    if output is not empty, open driver+xinit(filename)
//    send to mcplot_scan(s, plot_type)
//    if output is not empty close xend()
//    return
endfunction

