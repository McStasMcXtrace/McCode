function fontsize=setFontSize(vp)

	dim = xget('wdim');
	dim = mean(dim.*vp(3:4));

    if dim <= 300
      fontsize=0;
	elseif dim <= 700   // font size, depending on
	  fontsize=1;     // subplot state and window size
	else
	  fontsize=2;
	end
