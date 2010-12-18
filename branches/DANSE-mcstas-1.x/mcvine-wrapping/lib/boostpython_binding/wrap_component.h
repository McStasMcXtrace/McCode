// -*- C++ -*-
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//                                   Jiao Lin
//                      California Institute of Technology
//                        (C) 2005 All Rights Reserved
//
// {LicenseText}
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//


#include <boost/python.hpp>
#include "mcstas2/Component.h"


namespace mcstas2 {
  namespace boostpython_binding {
    
    /// wrap a mcstas component class
    /// Usage:
    ///  component_wrapper<component_t>::wrap( component_name, init< ... >() );
    /// Examples:
    ///  component_wrapper<Guide>::wrap( "guide", init< double, double, double >() );
    template <typename Component>
    struct component_wrapper
    {
      typedef boost::python::class_<
	Component, boost::python::bases< mcstas2::Component > > wrap;
    };
  }
}

// version
// $Id$

// End of file 
