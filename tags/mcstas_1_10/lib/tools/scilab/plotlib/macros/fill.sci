function fill(varargin)

[lhs,rhs]=argn(0);

if rhs==0
	x=[0 1 1]';y=[0 0 1]';c=[1 2 3]';
	fill(x,y,c,'facecolor','interp','axis','equal')
else
   mainPlot3d('fill',varargin);
end

endfunction /////
