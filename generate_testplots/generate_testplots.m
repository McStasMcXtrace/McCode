function generate_testplots(dir, ref, other)
% Generate SVG graphics andembed in html

% Add iFit etc. to path

addpath(genpath([getenv('HOME') '/iFit')])
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
    % Work through the refsims, generating SVG's and html
    for j=1:length(refsims)
        refsim=refsims{j};
        if (length(refsim>0))
            refdata=iData([ref '/' refsim '/mccode.sim']);
            rows=length(refdata);
            for k=1:rows
                thisref=refdata(k);
                if (not(isempty(thisref)))
                    if (not(exist([ref '/' refsim '/' thisref.Label '.svg'])))
                        if (sum(thisref(:)>1e200)==0)
                            plot(thisref); view([0 0 1]); axis tight; if not(any(size(thisref)==1)) colorbar; end
                            title([ref '/' refsim ' / ' thisref.Label]);
                            print('-dsvg', [ref '/' refsim '/' thisref.Label '.svg']);
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
                                if (not(exist([othersim '/' refsim '/' otherref.Label '.svg'])))
                                    if (sum(otherref(:)>1e200)==0)
                                        plot(otherref); view([0 0 1]); axis tight; if not(any(size(otherref)==1)) colorbar; end
                                        title([othersim '/' refsim ' / ' otherref.Label]);
                                        print('-dsvg', [othersim '/' refsim '/' otherref.Label '.svg']);                                        
                                    end 
                                end
                                if (all(size(thisref)==size(otherref)))
                                    diff = thisref - otherref;
                                    if (sum(diff(:)>1e200)==0)
                                        if (sum(otherref(:)>1e200)==0)
                                            plot(diff); view([0 0 1]); axis tight; if not(any(size(diff)==1)) colorbar; end
                                            title([otherref.Label ' difference to ref']);
                                            print('-dsvg', [othersim '/' refsim '/' otherref.Label '_diff.svg']);                                
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