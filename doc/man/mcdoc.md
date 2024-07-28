% MCDOC(1)
% McStas Neutron Ray-Tracing Team
% July 2024

# NAME

**mcdoc** - Creating and viewing the McStas library, component/instrument help and
Manuals

# SYNOPSIS

**mcdoc** [-h] [--install] [--dir DIR] [--manual] [--comps] [--web] [--verbose] [searchterm]

# DESCRIPTION

The **mcdoc** front-end generates html docpages from McStas instrument and
component files. A docpage is generated for every instrument and component
file, and an overview page is written and browsed. Just type `mcplot`.

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
:   open the mcstas website

**--verbose, -v**
:   prints a parsing log during execution

# FILES

/usr/share/mcstas/resources
/usr/share/mcstas/tools/Python/mccodelib/mccode_config.json
~/.mcstas/mccode_config.json

# EXAMPLES

Show documentation
:   - `mcdoc`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)
