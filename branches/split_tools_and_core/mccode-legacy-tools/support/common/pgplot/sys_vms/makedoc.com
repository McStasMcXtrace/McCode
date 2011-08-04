$! The following extracts documentation from pgplot source files.
$!
$! This procedure assumes the current configuration's drivers.list is
$! in the current default directory.
$!
$! The following files are created:
$!
$! pgplot.index - a listing of routine names and short descriptions,
$!                one routine per line
$! pgplot.doc -   documentation extracted from source files,
$!                one routine per page
$! pgplot.hlp -   VMS help file showing routines and drivers
$
$ on error then $ goto abort
$ on severe_error then $ goto abort
$ on control_y then $ goto abort
$ pgplot = p1
$ src = pgplot - "]" + ".src]"
$ file_list = "''src'pg*.f"
$ ff[0,8] = 12
$ tab[0,8] = 9
$ copy sys$input pgplot.hlp
1 PGPLOT
  PGPLOT GRAPHICS SUBROUTINE LIBRARY Version 5.1

  PGPLOT is a Fortran subroutine package for drawing graphs on a variety
  of display devices. For more details, see the manual ``PGPLOT Graphics
  Subroutine Library'' available from T. J. Pearson
  (tjp@astro.caltech.edu).

2 Routines

$ open/append hlp_file pgplot.hlp
$ create pgplot.hlp3
$ open/append hlp3_file pgplot.hlp3
$ create pgplot.index
$ open/append index_file pgplot.index
$ create pgplot.doc
$ open/append doc_file pgplot.doc
$ next_file:
$ file_name = f$search (file_list)
$ if file_name .eqs. "" then $ goto last_file
$ open/read src_file 'file_name'
$ read src_file title
$ if f$edit (f$extract (0, 2, title), "upcase") .nes. "C*" -
      then $ goto end_file
$ write index_file f$extract (2, 255, title)
$ write doc_file ff
$ write doc_file title
$ look_for_doc:
$ read/end=end_file src_file line
$ write doc_file line
$ if f$edit (f$extract (0, 2, line), "upcase") .nes. "C+" -
      then $ goto look_for_doc
$ write hlp_file  "  " + f$extract (2, 255, title)
$ write hlp3_file "3 " + f$element (0, " ", f$extract (2, 255, title))
$ write hlp3_file "  " + f$extract (2, 255, title)
$ read_doc:
$ read/end=no_end src_file line
$ write doc_file line
$ if f$edit (f$extract (0, 2, line), "upcase") .eqs. "C-" -
      then $ goto end_file
$ write hlp3_file " " + f$extract (1, 255, line)
$ goto read_doc
$
$ no_end:
$ write sys$output "No C- in ''file_name'"
$
$ end_file:
$ close src_file
$ goto next_file
$
$ last_file:
$ close hlp_file
$ close hlp3_file
$ close index_file
$ close doc_file
$ append pgplot.hlp3 pgplot.hlp
$ delete pgplot.hlp3;*
$
$ open/read drivers_file drivers.list
$ open/append hlp_file pgplot.hlp
$ write hlp_file "2 Drivers"
$ write hlp_file "  The following drivers are supported on this system:"
$ write hlp_file ""
$ write hlp_file "  File       Code       Description"
$ open/read drivers_file drivers.list
$ next_driver:
$ read/end=last_driver drivers_file driver
$ if f$extract (0, 1, driver) .nes. " " then $ goto next_driver
$ len = f$locate (tab, driver) ! tab is end of line
$ write hlp_file " " + f$extract (1, len-1, driver)
$ goto next_driver
$ last_driver:
$ close hlp_file
$ close drivers_file
$ exit (1)
$
$ abort:
$ write sys$output file_name
$ close src_file
$ close drivers_file
$ close hlp_file
$ close hlp3_file
$ close index_file
$ close doc_file
$ exit (0)
