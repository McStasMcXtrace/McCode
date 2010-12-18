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

#include "boost/python.hpp"
#include "mcstas2/Component.h"


namespace wrap_mcstas2 {

  void wrap_mcstas2_Component() 
  {
   
    using namespace boost::python;
    typedef mcstas2::Component w_t;

    class_<w_t, boost::noncopyable>
      ("McStasComponent", no_init);
  }

}

// version
// $Id$

// End of file 
