mode(-1)
path=get_absolute_file_path('loader.sce')
[unit,err]=mopen(path+'macros/lib','r');
mclose(unit);
if err
   exec('builder.sce',-1)
end
load(path+'macros/lib')
Title='Matlab-like graphic library'
if find(%helps(:,2)==Title)==[] then
  %helps=[%helps;path+'man',Title]
end
PLOTLIB=path+'macros/';
disp('loading plotlib version '+plotlibver())
clear path Title
set old_style on


