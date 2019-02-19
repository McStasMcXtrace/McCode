function McTest_compare(dirA,dirB)
% Function to generate a plot-report from two McCode 'selftest'
% directories

filesA=dir(dirA);
filesB=dir(dirB);

dirsA=filesA([filesA.isdir]);
dirsB=filesB([filesB.isdir]);


if (length(dirsA)==length(dirsB))
    for j=1:length(dirsA)
        if exist([dirsA(j).folder '/' dirsA(j).name '/mccode.sim' ], 'file')
            McCode_compare([dirsA(j).folder '/' dirsA(j).name],[dirsB(j).folder '/' dirsB(j).name]);
        end
    end
end