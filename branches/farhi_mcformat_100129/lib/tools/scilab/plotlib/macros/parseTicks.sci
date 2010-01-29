function [ticks]=parseTicks(funcName,parseList)

if length(parseList) == 1
   error(sprintf('%s : missing ticks spec',funcName));
end

select type(parseList(2))

case 10 //  a string
  select parseList(2)
    case 'off'
       ticks='none';
    case 'none'
       ticks='none';
    case 'on'
       ticks='on';
    else
       error(sprintf('%s : %s is an unknown ticks spec',funcName,parseList(2)));
    end
else
   error(sprintf('%s : missing ticks spec',funcName));
end
endfunction
