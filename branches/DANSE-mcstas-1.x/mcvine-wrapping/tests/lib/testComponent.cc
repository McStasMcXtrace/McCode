#include <iostream>
#include <cassert>
#include <string>
#include "mcstas2/mcstas2.h"

int main()
{
  using namespace mcstas2;
  using namespace std;

  Gravity g(0,0,1);

  class Sample : public Component {
  public:
    Sample( const char * name ): Component(name) {}
    virtual void trace(double & x,double & y,double & z,
		       double & vx,double & vy,double & vz,
		       double & t,
		       double & s1,double & s2,
		       double & p) 
    {}
  };
  Sample sample("sample");

  assert (string(sample.name()) == "sample"); 
  assert (!sample.gravityIsOn());
  
  sample.subjectTo( g );
  assert (sample.gravityIsOn());

  std::cout << ">> Test of class 'Component' passed" << std::endl;
}
