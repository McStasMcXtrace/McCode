function [not_tested, tested, instr_code, comps, instrs] = check_comp_tested(pw)
% check_comp_tested: test components wether they are used in example instruments.
%
% Example: check_comp_tested McCode/mcstas-comps
%
% input:
%  pw: parent directory to be tested. Is searched recursively for .comp and .instr.
%      when missing, the current directory is used.
% output:
%  not_tested: list of components which are not tested (cellstr)
%  tested:     list of components which are tested (cellstr)


if nargin == 0, pw = [];  end
if isempty(pw), pw = pwd; end

not_tested = {};
tested     = {};

allfiles = getAllFiles(pw);

comps  = {};
instrs = {};

% sort into components and instruments
for index=1:numel(allfiles)
  [p,f,e] = fileparts(allfiles{index});
  switch lower(e)
  case '.comp'
    comps{end+1} = allfiles{index};
  case '.instr'
    instrs{end+1} = allfiles{index};
  end
end

% load all instrument source code into memory for fast search
instr_code = [];
for index=1:numel(instrs)
  this = fileread(instrs{index});
  if ~isempty(this)
    instr_code{end+1} = this;
  end
end

% check if components appear in any instrument
for index=1:numel(comps)
  [p,f,e] = fileparts(comps{index});
  occurences = strfind(instr_code, f);
  
  if all(cellfun(@isempty, occurences))
    not_tested{end+1} = comps{index};
  else
    tested{end+1}     = comps{index};
  end
end

end % function

% ------------------------------------------------------------------------------

function fileList = getAllFiles(dirName)

  dirData = dir(dirName);      % Get the data for the current directory
  dirIndex = [dirData.isdir];  % Find the index for directories
  fileList = {dirData(~dirIndex).name}';  %'# Get a list of the files
  if ~isempty(fileList)
    fileList = cellfun(@(x) fullfile(dirName,x),...  % Prepend path to files
                       fileList,'UniformOutput',false);
  end
  subDirs = {dirData(dirIndex).name};  % Get a list of the subdirectories
  validIndex = ~ismember(subDirs,{'.','..'});  % Find index of subdirectories
                                               %   that are not '.' or '..'
  for iDir = find(validIndex)                  % Loop over valid subdirectories
    nextDir = fullfile(dirName,subDirs{iDir});    % Get the subdirectory path
    fileList = [fileList; getAllFiles(nextDir)];  % Recursively call getAllFiles
  end
  
end % getAllFiles
