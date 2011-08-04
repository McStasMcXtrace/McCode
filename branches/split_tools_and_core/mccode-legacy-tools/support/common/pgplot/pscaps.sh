#!/bin/csh
#
# Move BoundingBox statement in PostScript files from epilogue to
# prologue. This is useful for use with some applications which don't
# fully implement the Adobe DSC for Encapsulated PostScript (eg. they
# don't support a deferred bounding box specification using
# %%BoundingBox: (atend)
#
# Version 1.0: RGW 12-Apr-1994
# Version 1.1: RGW 29-Jan-1995
#
# Usage: pscaps <list of files>
#
#
set version="1.1"
set tmpfile=pscaps.tempfile
unalias mv
echo "PSCaps Version $version (rgw 29-Jan-1995)"
foreach file ($*)
  if ( -e $file ) then
    echo "Processing file $file..."
    set truebox=`fgrep '%%BoundingBox:' $file|fgrep -v atend`
    set atline=`fgrep -n '%%BoundingBox: (atend)' $file|awk -F':' '{print $1}'`
    if ($atline == "" ) then
      echo "File '$file' does not contain %%BoundingBox: (atend) construct"
      echo "Not a PGPLOT file, or already pscaps'ed"
    else
      @ prologue = $atline - 1
      @ restoffile = $atline + 1
      head -$prologue $file >! $tmpfile
      echo $truebox >> $tmpfile
      tail +${restoffile}l $file >> $tmpfile
      mv $tmpfile $file
    endif
  else
    echo "File '$file' does not exist"
  endif
end
exit
