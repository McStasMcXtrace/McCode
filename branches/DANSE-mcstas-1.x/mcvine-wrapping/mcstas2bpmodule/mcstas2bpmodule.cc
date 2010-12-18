// -*- C++ -*-
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//                                   Jiao Lin
//                      California Institute of Technology
//                      (C) 2005-2007 All Rights Reserved
//
// {LicenseText}
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//


#include <boost/python.hpp>

namespace wrap_mcstas2{
  void wrap_mcni_integration_Component();
  void wrap_mcstas2_Component();
  void wrap_random_numbers();
}


BOOST_PYTHON_MODULE(mcstas2bp)
{
  using namespace boost::python;
  using namespace wrap_mcstas2;

  wrap_mcni_integration_Component();
  wrap_mcstas2_Component();
  wrap_random_numbers();
}


// version
// $Id$

// End of file 
