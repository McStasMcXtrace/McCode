/*******************************************************************************
* Misc. useful routines to handle Cartesian coordinates.
*
*	Project: Monte Carlo Simulation of Triple Axis Spectrometers
*	File name: coords.c
*
*	Author: K.N.			Aug  8, 1997
*
*	$Id: coords.c,v 1.5 2003-01-21 07:50:44 pkwi Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.3  2000/07/27 09:06:11  kn
*	Changed argument of exp_number() from 0 to "0.0".
*
*	Revision 1.2  1998/10/02 08:36:05  kn
*	Fixed header comment.
*
*	Revision 1.1  1997/08/13 09:13:18  kn
*	Initial revision
*
*
* Copyright (C) Risoe National Laboratory, 1997-1998, All rights reserved
*******************************************************************************/

#include "mcstas.h"

/*******************************************************************************
* Since we use a lot of geometric calculations using Cartesian coordinates,
* we collect some useful routines here. However, it is also permissible to
* work directly on the underlying struct coords whenever that is most
* convenient (that is, the type Coords is not abstract).
*
* Coordinates are also used to store rotation angles around x/y/z axis.
*
* Since coordinates are used much like a basic type (such as double), the
* structure itself is passed and returned, rather than a pointer.
*
* At compile-time, the values of the coordinates may be unknown (for example
* a motor position). Hence coordinates are general expressions and not simple
* numbers. For this we used the type Coords_exp which has three CExp
* fields. For runtime (or calculations possible at compile time), we use
* Coords which contains three double fields.
*******************************************************************************/

/* Get all-zero coordinate. */
Coords
coords_origo(void)
{
  Coords c;
  
  c.x = 0;
  c.y = 0;
  c.z = 0;
  return c;
}

Coords_exp
coords_exp_origo(void)
{
  Coords_exp c;
  
  c.x = exp_number("0.0");
  c.y = exp_number("0.0");
  c.z = exp_number("0.0");
  return c;
}

/* Add two coordinates. */
Coords
coords_add(Coords a, Coords b)
{
  Coords c;

  c.x = a.x + b.x;
  c.y = a.y + b.y;
  c.z = a.z + b.z;
  return c;
}
