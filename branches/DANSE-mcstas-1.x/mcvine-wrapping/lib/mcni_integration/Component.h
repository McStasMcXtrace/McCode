// -*- C++ -*-
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//                                   Jiao Lin
//                      California Institute of Technology
//                         (C) 2005 All Rights Reserved  
//
// {LicenseText}
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//


#ifndef H_MCSTAS2_MCNI_INTEGRATION_COMPONENT
#define H_MCSTAS2_MCNI_INTEGRATION_COMPONENT

#include "mcni/AbstractNeutronComponent.h"
#include "mcni/neutron.h"
#include "mcstas2/Component.h"

namespace mcstas2{

  namespace mcni_integration{

    using namespace mcni;


    struct Component: public mcni::AbstractNeutronComponent {
    public:
      
      // meta-methods
      inline Component( mcstas2::Component & mcstas_component );
      
      // methods
      inline virtual void scatter(Neutron::Event & ev);

      // data
      mcstas2::Component & mcstas_core;
    };

  }
} // mcstas2



mcstas2::mcni_integration::Component::Component
(mcstas2::Component & mcstas_component)
  : mcni::AbstractNeutronComponent( mcstas_component.name() ),
    mcstas_core( mcstas_component )
{
}


void mcstas2::mcni_integration::Component::scatter
(Neutron::Event & ev)
{
  const mcni::Neutron::State & state = ev.state;
  mcstas_core.trace
    ( state.position.x, state.position.y, state.position.z,
      state.velocity.x, state.velocity.y, state.velocity.z,
      ev.time, 
      (double &)state.spin.s1, (double &)state.spin.s2,
      ev.probability
      );
}


#endif //H_MCSTAS2_MCNI_INTEGRATION_COMPONENT


// version
// $Id$

// End of file 
