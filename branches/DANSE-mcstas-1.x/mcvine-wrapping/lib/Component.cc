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


#include <iostream>

#include "Component.h"
#include "random_numbers.h"


mcstas2::Component::Component( const char * name )
  : m_name(name), m_gravity_on(0) 
{}


mcstas2::Component::Component( ) 
  : m_name("no name"), m_gravity_on(0)
{}

mcstas2::Component:: ~Component()
{ finalize(); }


const char *
mcstas2::Component::name() const 
{ return m_name.c_str(); }


void mcstas2::Component:: subjectTo
( const Gravity & g )
{ m_g = g; m_gravity_on = 1; }


bool mcstas2::Component::gravityIsOn() const
{ return m_gravity_on; }


const mcstas2::Gravity & 
mcstas2::Component::gravity() const 
{ return m_g; }


void 
mcstas2::Component::setName( const char * name ) 
{ m_name = name; }


void mcstas2::Component::save()
{
}

void mcstas2::Component::finalize()
{
  std::cout << "finalize component \"" << name() << '"' << std::endl;
}


// version
// $Id$

// End of file 
