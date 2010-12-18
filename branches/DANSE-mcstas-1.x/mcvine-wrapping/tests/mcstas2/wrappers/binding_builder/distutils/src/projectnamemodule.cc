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

#include "exceptions.h"
#include "bindings.h"


char pyprojectname_module__doc__[] = "";

// Initialization function for the module (*must* be called initprojectname)
extern "C"
void
initprojectname()
{
    // create the module and add the functions
    PyObject * m = Py_InitModule4(
        "projectname", pyprojectname_methods,
        pyprojectname_module__doc__, 0, PYTHON_API_VERSION);

    // get its dictionary
    PyObject * d = PyModule_GetDict(m);

    // check for errors
    if (PyErr_Occurred()) {
        Py_FatalError("can't initialize module projectname");
    }

    // install the module exceptions
    pyprojectname_runtimeError = PyErr_NewException("projectname.runtime", 0, 0);
    PyDict_SetItemString(d, "RuntimeException", pyprojectname_runtimeError);

    return;
}

// version
// $Id$

// End of file
