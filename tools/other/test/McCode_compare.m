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
            dirB=strrep(dirB,'/','+');
            dirA=strrep(dirA,'/','+');

            fig=figure
            for j=1:length(A)
                figure(fig)
                subplot(rows,3,(j-1)*2+j)
                plot(A(j)); view([0 0 1]); axis tight; if not(any(size(A(j))==1)) colorbar; end
                title(['A - ' num2str(j)])
                subplot(rows,3,(j-1)*2+j+1)
                plot(B(j)); view([0 0 1]); axis tight; if not(any(size(A(j))==1)) colorbar; end
                title(['B - ' num2str(j)])
                subplot(rows,3,(j-1)*2+j+2)
                plot(A(j)-B(j)); view([0 0 1]); axis tight; if not(any(size(A(j))==1)) colorbar; end
                figure
                title(['diff - ' num2str(j)])
                subplot(1,3,1)
                plot(A(j)); view([0 0 1]); axis tight; if not(any(size(A(j))==1)) colorbar; end
                title(['A - ' num2str(j)])
                subplot(1,3,2)
                plot(B(j)); view([0 0 1]); axis tight; if not(any(size(A(j))==1)) colorbar; end
                title(['B - ' num2str(j)])
                subplot(1,3,3)
                plot(A(j)-B(j)); view([0 0 1]); axis tight; if not(any(size(A(j))==1)) colorbar; end
                title(['diff - ' num2str(j)])
                [FILEPATH,NAME,EXT] = fileparts(A(j).filename);
                sgtitle(strvcat('Comparison A vs B: ',['A: ' dirA],['B: ' dirB], ['Monitor: ' NAME EXT]), 'interpreter','none')
                print(gcf, '-dpdf', ['Subplots_' num2str(j) '_' dirA '_vs_' dirB '.pdf']);
            end
            figure(fig)
            sgtitle(strvcat('Comparison A vs B: ',['A: ' dirA],['B: ' dirB]), 'interpreter','none')
            print(fig, '-dpdf', ['Comparison_' dirA '_vs_' dirB '.pdf']);
            close all
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