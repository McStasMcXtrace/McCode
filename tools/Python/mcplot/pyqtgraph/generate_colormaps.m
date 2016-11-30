% This Matlab script generates a colormap.txt file for insertion in plotfuncs.py
maps={'autumn','bone','colorcube','cool','copper','flag','gray', ...
      'hot','hsv','jet','lines','parula','pink','prism','spring','summer','winter'};

imagesc
fileID = fopen('colormaps.txt','w');
fprintf(fileID,'    colormaps={\n');
for j=1:length(maps)
    map=maps{j};
    eval(['colormap ' map])
    cmap=colormap;
    cmap=round(255*cmap);
    siz=size(cmap);
    
    fprintf(fileID,'        ''%s''  : np.array([',map);
    
    for k=1:siz(1)
        if k<siz(1)
            fprintf(fileID,'[%3i, %3i, %3i, 255], ',cmap(k,1),cmap(k,2),cmap(k,3));
        else
            fprintf(fileID,'[%3i, %3i, %3i, 255]',cmap(k,1),cmap(k,2),cmap(k,3));
        end
    end
    fprintf(fileID,'], dtype=np.ubyte),\n');
end
fprintf(fileID,'        }\n');
fclose(fileID);