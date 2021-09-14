function [results,instruments,ref,datasets]=compare_compile(testdir,refdir)
set(0,'defaultAxesFontSize',14)
% How many ref / test datasets?

% Get a list of all files and folders in this folder.
files = dir(testdir);

counter=0;

% Print folder names to command window.
for k = 1 : length(files)
    if files(k).isdir
        f=files(k);
        if strcmp(f.name,'.');
            files(k).isdir=0;
        elseif strcmp(f.name,'..')
            files(k).isdir=0;
        else
            if strcmp(f.name,refdir)
                jsontext = fileread([testdir '/' f.name '/testresults_' f.name '.json']);
                refname=f.name;
                ref=jsondecode(jsontext);
                files(k).isdir=0;
            else
                counter=counter+1;
                jsontext = fileread([testdir '/' f.name '/testresults_' f.name '.json']);
                datasets{counter}=jsondecode(jsontext);
            end
        end
    end
end


isdir=[files.isdir];
dirnames=files(isdir==1);
dirnames=strvcat(dirnames.name);

instruments=fieldnames(ref);
results=zeros(length(instruments),length(datasets));
for k=1:length(instruments)
    if not(strcmp(instruments{k}, 'x_meta'))
        refinst=getfield(ref,instruments{k})
        reftime=refinst.compiletime;
        
        if not(isempty(reftime)) && isnumeric(reftime)
            if not(refinst.testval==-1)
                for l=1:length(datasets)
                    if isfield(datasets{l},instruments{k})
                        thisinst=getfield(datasets{l},instruments{k})
                        if not(isempty(thisinst.compiletime)) && isnumeric(thisinst.compiletime)
                            results(k,l)=thisinst.compiletime./refinst.compiletime;
                        else
                            results(k,l)=NaN;
                        end
                    else
                        results(k,l)=NaN;
                    end
                end
            else
                results(k,:)=NaN;
            end
        else
            results(k,:)=NaN;
        end
    else
        results(k,:)=NaN;
    end
end

figure
hold on
ctr=1;
plot([1 length(instruments)],[1 1],'--');
legends{ctr}=['baseline: ' refname];
for k=1:length(datasets)
    h=semilogy(results(:,k))
    nonnan=results(:,k);
    nonnan(isnan(nonnan))=[];
    nonnan(isinf(nonnan))=[];
    xl=get(gca,'xlim');
    h1=plot(xl,[mean(nonnan) mean(nonnan)]);
    ctr=ctr+1;
    legends{ctr}=dirnames(k,:);
    ctr=ctr+1;
    legends{ctr}=['mean=' num2str(mean(nonnan)) ' min=' num2str(min(nonnan)) ' max=' num2str(max(nonnan)) ];
    set(h1,'color',get(h,'color'),'linestyle',':')
end
h=legend(legends);
set(h,'interpreter','none','FontSize',7);
xlabel('Instrument no.')
h=ylabel(['Compilation time wrt. ' refname]);
set(h,'interpreter','none');
h=title(['Compilation time wrt. ' refname]);
set(h,'interpreter','none');
set(gca,'yscale','log')

eval(['print -dpng ' testdir '_compilation.png']);