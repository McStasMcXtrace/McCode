#include <iostream>
#include <string>
#include <cassert>
#include "mcstas2/mcstas2.h"

using namespace mcstas2;
using namespace std;

// repeativity of random numbers
void test1()
{
  srandom( 1 );
  double r11 = rand01(), r12 = rand01();

  srandom( 1 );
  double r21 = rand01(), r22 = rand01();

  assert (r11==r21);
  assert (r12==r22);
  assert (r11!=r12);
}

// effect of seeding
void test2()
{
  srandom( 1 );
  double r11 = rand01(), r12 = rand01();

  srandom( 2 );
  double r21 = rand01(), r22 = rand01();

  assert (r11!=r21);
  assert (r12!=r22);

  assert (r11!=r12);
  assert (r21!=r22);
}


int main()
{
  test1();
  test2();
}
