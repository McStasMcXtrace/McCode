/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: coords.c
*
* %Identification
* Written by: K.N.
* Date: Aug  8, 1997
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision$
*
* Misc. useful routines to handle Cartesian coordinates.
*
*******************************************************************************/

#include "mccode.h"

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

Coords_exp coords_exp_origo(void)
{
  Coords_exp c;

  c.x = exp_number("0.0");
  c.y = exp_number("0.0");
  c.z = exp_number("0.0");
  return c;
}
