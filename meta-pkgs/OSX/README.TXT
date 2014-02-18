This folder contains pmdoc's for use with PackageMaker on OSX - for assembling the CMake/CPack -created sub-packages to Metapackages.

To update or add packages to these pmdocs you should:

a) Run e.g. toplevel ./build_macos_Mavercics_mcstas 2.x meta

b) Do manual edits using PackageMaker to the metapackage generated in trunk/dist

c) Using e.g. sed replace 2.x by @VERSION@ in the generated .xml's

d) Using e.g. sed replace any absolute path like e.g. /Users/mccode/McCode/trunk/dist by @DISTDIR@ in the generated .xml's

e) Copy back the xml's to the SVN-controlled version (here) and add missing + commit changed files
