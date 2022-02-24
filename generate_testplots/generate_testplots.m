function generate_testplots(dir, ref, other)
% Generate PNG graphics andembed in html

% Add iFit etc. to path

opengl('save','software')
addpath(genpath([getenv('HOME') '/iFit']))
currdir=pwd;
addpath(currdir)

% Change directory
cd(dir)

% Start by locating ref and other datasets
if exist(ref,'dir') == 7
    % other datasets
    %[tmp, other]=unix(['ls | grep -v ' ref ' | grep -v html | grep -v \.md | grep -v  \.txt']);
    %other=split(other);
    %cols=length(other);
    %display(other);
    % Look for datasets with actual output in ref
    [tmp, refsims] = unix(['find ' ref ' -name mccode.sim | cut -f2-3 -d/']);
    refsims=split(refsims);
    % Work through the refsims, generating PNG's and html
    for j=1:length(refsims)
        refsim=refsims{j};
        if (length(refsim>0))
            refdata=iData([ref '/' refsim '/mccode.sim']);
            rows=length(refdata);
            for k=1:rows
                thisref=refdata(k);
                if (not(isempty(thisref)))
                    % Check for NaN and max val's
                    nans=any(isnan(thisref(:)));
                    Max = max(abs(thisref(:)));
                    if (not(nans))
                        if (Max < 1e200)
                            plot(thisref); view([0 0 1]); axis tight; if not(any(size(thisref)==1)) colorbar; end
                            title([ref '/' refsim ' / ' thisref.Label]);
                            display(['print -dpng ' ref '/' refsim '/' thisref.Label '.png']);
                            eval(['print -dpng ' ref '/' refsim '/' thisref.Label '.png']);
                        end
                    end
                end
                othersim=other;%{l};
                if (length(othersim>0))
                    if exist([othersim '/' refsim ],'dir') == 7
                        otherdata=iData([othersim '/' refsim '/mccode.sim']);
                        if (length(otherdata) == length(refdata))
                            otherref=otherdata(k);
                            if (not(isempty(otherref)))
                                % Check for NaN and max val's
                                nans=any(isnan(otherref(:)));
                                Max = max(abs(otherref(:)));
                                if (not(nans))
                                    if (Max < 1e200)
                                        plot(otherref); view([0 0 1]); axis tight; if not(any(size(otherref)==1)) colorbar; end
                                        title([othersim '/' refsim ' / ' otherref.Label]);
                                        eval(['print -dpng ' othersim '/' refsim '/' thisref.Label '.png']);
                                    end
                                end
                                if (all(size(thisref)==size(otherref)))
                                    diff = thisref - otherref;
                                    if (not(isempty(diff)))
                                        % Check for NaN and max val's
                                        nans=any(isnan(diff(:)));
                                        Max = max(abs(diff(:)));
                                        if (not(nans))
                                            if (Max < 1e200)
                                                plot(diff); view([0 0 1]); axis tight; if not(any(size(diff)==1)) colorbar; end
                                                title([otherref.Label ' difference to ref']);
                                                eval(['print -dpng ' othersim '/' refsim '/' thisref.Label '_diff.png']);
                                            end                                       
                                        end
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end
    display(refsims)
else
    display(['Reference ' ref ' does not exist in ' dir])
end

cd(currdir)
