mode(-1)
pathB=get_absolute_file_path('builder.sce')
if isdef('plotlib')
   predef(0);
   clear plotlib
end
getf(pathB+'macros/plotlibver.sci');
disp('Building plotlib '+plotlibver()+' in ' +pathB+'macros')
genlib('plotlib',pathB+'macros',%t)
clear pathB
