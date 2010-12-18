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

#include "bindings.h"

#include "misc.h"          // miscellaneous methods

// the method table

struct PyMethodDef pyprojectname_methods[] = {

    // dummy entry for testing
    {pyprojectname_hello__name__, pyprojectname_hello,
     METH_VARARGS, pyprojectname_hello__doc__},

    {pyprojectname_copyright__name__, pyprojectname_copyright,
     METH_VARARGS, pyprojectname_copyright__doc__},


// Sentinel
    {0, 0}
};

// version
// $Id$

// End of file
