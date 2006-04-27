function fontsize=setFontSize(vp)

	dim = xget('wdim');
	dim = mean(dim.*vp(3:4));

    if dim <= 500
      fontsize=1;
	elseif dim <= 700   // font size, depending on
	  fontsize=2;     // subplot state and window size
	else
	  fontsize=3;
	end
	
endfunction
