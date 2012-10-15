function ind=findColorIndex(col)
c=xget('colormap');
ind=find(and([c(:,1)==col(1) c(:,2)==col(2) c(:,3)==col(3)],'c'));
if ind==[]
   ind=addcolor(col);
end
endfunction
