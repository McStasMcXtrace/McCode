* README.TXT

This folder contains basic MCPL infrastructure, including example of how to write and read data.

Compilation commands:
cc -std=c99 -o mcplwrite writeexample.c mcpl.c
cc -std=c99 -o mcpltool tool.c mcpl.c 
cc -std=c99 -o mcplread readexample.c mcpl.c 

will give you the binaries mcplwrite mcplread and mcpltool.

Run ./mcplwrite lala.mcpl to generate an example MCPL file

Once proper McStas components have been written, the folder contents should be placed in the mcstas/mcxtrace package dirs
