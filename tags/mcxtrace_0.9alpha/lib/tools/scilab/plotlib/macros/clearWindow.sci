function clearWindow(win,typeOfPlot,cmap)

	[lhs,rhs]=argn(0);

	state=loadGraphicState(win);

	xtape('clear',win);
	
	if rhs==2
		if 	typeOfPlot=="pcolor" | ...
        	typeOfPlot=="fill" | ...
        	typeOfPlot=="bar" | ...
			typeOfPlot=="surf" | ...
			typeOfPlot=="surfl" | ...
			typeOfPlot=="tripcolor" | ...
			typeOfPlot=="trisurf" | ...
			typeOfPlot=="trisurfl" | ...
			typeOfPlot=="fill3"

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

	xset('foreground',findColorIndex(state('foreground')));
	xset('background',findColorIndex(state('frameColor')));

	if xget('pixmap')==0
		xclear(win)
    else
		xset('wwpc')
	end
	
//	xset('foreground',findColorIndex(state('foreground')));
//	xset('background',findColorIndex(state('frameColor')));

endfunction
