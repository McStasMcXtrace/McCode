function McCode_compare(dirA,dirB)
% Function to generate a plot-report from two supposedly identical
% datasets

% checks that these are indeed directories?
if (isfolder(dirA) && isfolder(dirB))
    Astr=[dirA '/mccode.sim'];
    Bstr=[dirB '/mccode.sim'];
    if ( exist(Astr, 'file') == 2 && exist(Bstr, 'file') == 2 )
        A=iData(Astr);
        B=iData(Bstr);

        % checks that these are identical length etc.?
        if (length(A)==length(B))
            C=A-B;
            rows=length(A);

            fig=figure
            for j=1:length(A)
                subplot(rows,3,(j-1)*2+j)
                plot(A(j)); view([0 0 1]); axis tight
                title(['A - ' num2str(j)])
                subplot(rows,3,(j-1)*2+j+1)
                plot(B(j)); view([0 0 1]); axis tight
                title(['B - ' num2str(j)])
                subplot(rows,3,(j-1)*2+j+2)
                plot(A(j)-B(j)); view([0 0 1]); axis tight
                title(['diff - ' num2str(j)])
            end

            sgtitle(strvcat('Comparison A vs B: ',['A: ' dirA],['B: ' dirB]), 'interpreter','none')
            dirB=strrep(dirB,'/','_');
            dirA=strrep(dirA,'/','_');
            print(fig, '-dpdf', ['Comparison_' dirA '_vs_' dirB '.pdf']);
            close
        else
            display('And the datasets should be of the same dimension!');
        end
    else
        display(['Both folders should be McCode datasets containing ' ...
                 'a mccode.sim file!']);
    end
else
    display('I need two folders as input!');
end