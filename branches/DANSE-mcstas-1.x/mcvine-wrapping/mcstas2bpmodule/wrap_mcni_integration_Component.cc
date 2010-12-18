// -*- C++ -*-
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//                                   Jiao Lin
//                      California Institute of Technology
//                        (C) 2007  All Rights Reserved
//
// {LicenseText}
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//

#include "mcstas2/mcni_integration/Component.h"
#include "mcni/boostpython_binding/wrap_component.h"

namespace wrap_mcstas2 {

  mcstas2::Component & get_core( mcstas2::mcni_integration::Component & c )
  {
    return c.mcstas_core;
  }

  void wrap_mcni_integration_Component() 
  {
    using namespace mcni::boostpython_binding;
    typedef mcstas2::mcni_integration::Component w_t;

    boostpython_binding::component_wrapper<w_t>::wrap
      ("McStasComponentAsMcniComponent", 
       init<mcstas2::Component &>()
       [with_custodian_and_ward<1,2> () ]
       )
      .def( "core", &get_core, 
	    return_internal_reference< 1 >() )
      ;
  }

}

// version
// $Id$

// End of file 
