function setCurrentViewport(state,win)

	if length(state('axis'))==4 // 2D case
		xsetech(wrect=state('ech'),frect=state('axis'),arect=[0 0 0 0]);  
	else // 3D case
		xsetech(wrect=state('ech'),arect=[0 0 0 0]);  
	end

endfunction
