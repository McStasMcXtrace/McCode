function clearWindow(win,typeOfPlot,cmap)

	[lhs,rhs]=argn(0);

	if rhs==1
//		xtape('clear',win);
//		xset('wwpc');
//		if xget('pixmap')==0
//			xclear(win)
//		end
		xbasc();
		return		
	end

	state=loadGraphicState(win);

//	xtape('clear',win)
	xbasc()
	
	if rhs==2
		if 	typeOfPlot=="pcolor" | ...
			typeOfPlot=="surf" | ...
			typeOfPlot=="surfl" | ...
			typeOfPlot=="tripcolor" | ...
			typeOfPlot=="trisurf" | ...
			typeOfPlot=="trisurfl"

			if length(state('RGBcolormaptable'))==0
				RGBmap=state('RGBcolormap');
				n1=size(state('colormap'),1);
				n2=size(RGBmap,1);
				finalColormap=[state('colormap');RGBmap];
				state('RGBcolormaptable')=n1+1:n1+n2;
				saveGraphicState(state,win);
				xset('colormap',finalColormap);
			end
		elseif typeOfPlot=="fig"
			if length(state('RGBcolormaptable'))==0
				finalColormap=[state('colormap')];
				xset('colormap',finalColormap);
			end
		elseif typeOfPlot=="clf"
		else
			if length(state('RGBcolormaptable'))==0
				finalColormap=[state('colormap')];
				xset('colormap',finalColormap);
			end
		end
	end
	
	xset('foreground',addcolor(state('foreground')));
	xset('background',addcolor(state('frameColor')));
//	xset('wwpc');

endfunction
