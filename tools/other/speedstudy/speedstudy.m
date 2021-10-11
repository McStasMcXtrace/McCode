function speedstudy(dirname)

cd(dirname)

[tmp,output]=unix('ls -rt | grep -v instr | grep -v \.eps');

output=splitlines(output);
siz=size(output);
for j=1:siz(1)-1
  if(length(output{j})>0)
    display(['Working on ' output{j}]); 
    if j==1
      base=load([output{j} '/results.dat']);
      figure
      plot(base(:,1),base(:,2)./base(:,2))
      set(gca,'xscale','log','yscale','log')
      hold on
    else
      data=load([output{j} '/results.dat']);
      extra=base;
      sbase=length(base);
      sdata=length(data);
      if sbase < sdata
        for k=1:sdata-sbase
	  extra=[extra; 10*extra(sbase+k-1,:)]
        end
      end
      plot(data(:,1),extra(:,2)./data(:,2))
    end
  end
end
xlabel('Problem size (log)')
ylabel('Relative speedup (log)')
h=title([dirname ' - max ' num2str(extra(end,2)./data(end,2))],'interpreter','none')
legend(output{1:end-1},'interpreter','none')
print([dirname '.eps'],'-depsc')
cd ..
