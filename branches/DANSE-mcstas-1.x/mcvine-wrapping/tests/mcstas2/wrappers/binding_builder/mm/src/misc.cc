// -*- C++ -*-
// 
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 
//                               Michael A.G. Aivazis
//                        California Institute of Technology
//                        (C) 1998-2005  All Rights Reserved
// 
//  <LicenseText>
// 
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 

#include <portinfo>
#include <Python.h>

#include "misc.h"


// copyright

char pyprojectname_copyright__doc__[] = "";
char pyprojectname_copyright__name__[] = "copyright";

static char pyprojectname_copyright_note[] = 
    "projectname python module: Copyright (c) 1998-2005 Michael A.G. Aivazis";


PyObject * pyprojectname_copyright(PyObject *, PyObject *)
{
    return Py_BuildValue("s", pyprojectname_copyright_note);
}
    
// hello

char pyprojectname_hello__doc__[] = "";
char pyprojectname_hello__name__[] = "hello";

PyObject * pyprojectname_hello(PyObject *, PyObject *)
{
    return Py_BuildValue("s", "hello");
}
    
// version
// $Id$

// End of file
