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
* Version: 1.3
*
* Misc. useful routines to handle Cartesian coordinates.
*
*	$Id: coords.c,v 1.9 2003-01-21 08:38:40 pkwi Exp $
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

Coords_exp
coords_exp_origo(void)
{
  Coords_exp c;
  
  c.x = exp_number("0.0");
  c.y = exp_number("0.0");
  c.z = exp_number("0.0");
  return c;
}
