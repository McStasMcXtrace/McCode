// -*- C++ -*-
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//                                   Jiao Lin
//                      California Institute of Technology
//                        (C) 2008  All Rights Reserved
//
// {LicenseText}
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//

#include "boost/python.hpp"
#include "mcstas2/random_numbers.h"

namespace wrap_mcstas2 {

  inline void _srandom( const unsigned int & seed ) {
    srandom( seed );
  }

  void wrap_random_numbers() 
  {
    using namespace boost::python;

    def( "srandom", &_srandom );
  }

}

// version
// $Id$

// End of file 
