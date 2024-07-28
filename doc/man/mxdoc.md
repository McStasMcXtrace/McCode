% MXDOC(1)
% McXtrace Team
% July 2024

# NAME

**mxdoc** - Creating and viewing the McXtrace library, component/instrument help and
Manuals

# SYNOPSIS

**mxdoc** [-h] [--install] [--dir DIR] [--manual] [--comps] [--web] [--verbose] [searchterm]

# DESCRIPTION

The **mxdoc** front-end generates html docpages from McXtrace instrument and
component files. A docpage is generated for every instrument and component
file, and an overview page is written and browsed. Just type `mxplot`.

# OPTIONS

**searchterm**
:   search filter or .instr/.comp file

**-h, --help**
:   show this help message and exit

**--install, -i**
:   generate installation master doc page

**--dir DIR, -d DIR**
:   add search results from this directory

**--manual, -m**
:   open the system manual

**--comps, -c**
:   open the component manual

**--web, -w**
:   open the mcxtrace website

**--verbose, -v**
:   prints a parsing log during execution

# FILES

/usr/share/mcxtrace/resources
/usr/share/mcxtrace/tools/Python/mccodelib/mccode_config.json
~/.mcxtrace/mccode_config.json

# EXAMPLES

Show documentation
:   - `mxdoc`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)
