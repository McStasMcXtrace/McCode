#include <iostream>
#include "mcstas2/mcstas2.h"

int main()
{
  using namespace mcstas2;

  Gravity g(0,0,1);

  assertNumberAlmostEqual( g.amplitude, 9.8 );
  assertNumberAlmostEqual( g.x, 0.0 );
  assertNumberAlmostEqual( g.y, 0.0 );
  assertNumberAlmostEqual( g.z, 9.8 );

  Gravity g1(1,0,0);

  assertNumberAlmostEqual( g1.amplitude, 9.8 );
  assertNumberAlmostEqual( g1.x, 9.8 );
  assertNumberAlmostEqual( g1.y, 0.0 );
  assertNumberAlmostEqual( g1.z, 0.0 );

  std::cout << ">> Test of class 'Gravity' passed" << std::endl;
}
