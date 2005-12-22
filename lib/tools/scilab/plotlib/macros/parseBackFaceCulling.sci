function [bfc]=parseBackFaceCulling(typeOfPlot,argList)

if length(argList)>=2
if type(argList(2))==10
   select argList(2)
   case 'yes'
      bfc='yes';
   case 'no'
      bfc='no';
   else
     error(sprintf('%s : unknown BackFaceCulling specification ''%s''',typeOfPlot,argList(2)))
   end
else
   error(sprintf('%s : BackFaceCulling specification must be a string',typeOfPlot))
end
else
    error(sprintf('%s : missing BackFaceCulling specification',typeOfPlot))
end

endfunction // end of parseBackFaceCulling
