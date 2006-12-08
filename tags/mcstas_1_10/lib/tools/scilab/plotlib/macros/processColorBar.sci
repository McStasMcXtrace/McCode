function processColorBar(table,typ,state)

ech=state('cbech');
x=ech(1);y=ech(2);w=ech(3);h=ech(4);

n=length(table);
ax=caxis();



if typ=='left' | typ=='right'

	xsetech(wrect=[x y w h],frect=[0 ax(1) 1 ax(2)],arect=[0 0 0 0]);
  xfpolys([0 1 1 0]',[ax(1) ax(1) ax(2) ax(2)]',[table(1) table(1) table($) table($)]');
	plotframe([0 ax(1) 1 ax(2)],[1 0 2 4],[%f,%f],['','','']);

elseif typ=='top' | typ=='bot'

	xsetech(wrect=[x y w h],frect=[ax(1) 0  ax(2) 1],arect=[0 0 0 0]);
  xfpolys([ax(1) ax(2) ax(2) ax(1)]',[0 0 1 1]',[table(1) table($) table($) table(1)]');
	plotframe([ax(1) 0 ax(2) 1],[2 4 1 0],[%f,%f],['','','']);
end
endfunction
