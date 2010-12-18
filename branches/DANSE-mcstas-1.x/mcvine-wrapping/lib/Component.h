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


#ifndef H_MCSTAS2_COMPONENT
#define H_MCSTAS2_COMPONENT

#include <string>

#include "Gravity.h"

namespace mcstas2{

  class Component{

  public:

    // types
    typedef unsigned int seed_t;

    // meta-methods
    Component( const char * name );
    Component( );
    virtual ~Component();

    // methods
    const char * name() const;
    // gravity related
    void subjectTo( const Gravity & g );
    bool gravityIsOn() const;
    const Gravity & gravity() const;

    /// finding trace of a neutron in myself
    virtual void trace
    (double & x,double & y,double & z,
     double & vx,double & vy,double & vz,
     double & t,
     double & s1,double & s2,
     double & p) = 0;
    
    /// save data. for monitors
    virtual void save( );
    /// finalize everything. this is almost just dtor.
    virtual void finalize( );

  protected:

    /// set this component's name. only called by subclass
    void setName( const char * name );

    // data
    std::string m_name;
    bool m_gravity_on;
    Gravity m_g;

  }; // Component

} // namespace mcstas2


#endif //H_MCSTAS2_COMPONENT


// version
// $Id$

// End of file 
