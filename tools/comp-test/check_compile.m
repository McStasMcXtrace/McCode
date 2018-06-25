function [failed, compiled] = check_compile(pw)
% check_compile: check that McCode instruments compile
%
% Example: check_compile McCode/mcstas-comps/examples
%
% input:
%  pw: path to 'examples' with '.instr' files. When missing, the current path
%      is used.
% output:
%  failed:   name of instruments that failed to compile (cellstr)
%  compiled: name of instruments that compiled (cellstr)

if nargin < 1, pw = pwd; end

% required to avoid Matlab to use its own libraries
if ismac,      precmd = 'DYLD_LIBRARY_PATH= ; DISPLAY= ; ';
elseif isunix, precmd = 'LD_LIBRARY_PATH= ; DISPLAY= ; '; 
else           precmd = ''; 
end

instr    = dir(fullfile(pw, '*.instr'));
compiled = {};
failed   = {};
for index=1:numel(instr)

  this = instr(index);
  cmd = [ precmd 'mcrun.pl -c -n0 ' fullfile(pw, this.name) ];
  disp(cmd);
  [status, result] = system(cmd);
  
  if status == 255 || status == 0 % OK
    compiled{end+1} = this.name;
  else % status is e.g 127 or 1
    failed{end+1} = this.name;
  end
  
end

disp([ 'Directory: ' pw ])
disp([ 'Failed:   ' num2str(numel(failed)) ]);
disp([ 'Compiled: ' num2str(numel(compiled)) ]);
