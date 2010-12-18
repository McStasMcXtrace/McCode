#include <iostream>
#include <vector>
#include <string>
#include "mcstas2/mcstas2.h"



using namespace mcstas2;
using namespace std;



int test_0D()
{

  class Detector : public Component {
  public:
    Detector( const char * name ): 
      Component(name), m_N(0), m_p(0), m_p2(0)
    {
      mcuse_format( "mcstas" );
    }

    ~Detector() {
      DETECTOR_OUT_0D( Component::name(), m_N, m_p, m_p2 );
    }
    
    virtual void trace(double & x,double & y,double & z,
		       double & vx,double & vy,double & vz,
		       double & t,
		       double & s1,double & s2,
		       double & p) 
    {
      m_N += 1; m_p += p; m_p2 += p*p;
    }

  private:
    double m_N, m_p, m_p2;
    
  };

  Detector *detector = new Detector("detector");

  double x,y,z, vx,vy,vz, t, s1, s2, p = 1.;
  detector->trace( x,y,z, vx,vy,vz, t, s1,s2, p );

  delete detector;

  std::cout << ">> Test of 'DETECTOR_OUT_0D' passed" << std::endl;
  return 0;
}




int test_1D()
{

  class Detector : public Component {
  public:
    Detector( const char * name ): 
      Component(name), m_N(10), m_p(10), m_p2(10)
    {
    }

    ~Detector() {
      mcuse_format( "mcstas" );
      mcuse_dir( "." );
      DETECTOR_OUT_1D( Component::name(), 
		       "vz", 
		       "Intensity",
		       "vz", 0, 5000, 10,
		       &m_N[0], &m_p[0], &m_p2[0], 
		       "vz.dat" );
    }
    
    virtual void trace(double & x,double & y,double & z,
		       double & vx,double & vy,double & vz,
		       double & t,
		       double & s1,double & s2,
		       double & p) 
    {
      double avz = fabs(vz);
      size_t i = avz/500.;
      if (i>9) return;
      m_N[i] += 1; m_p[i] += p; m_p2[i] += p*p;
    }

  private:
    std::vector<double> m_N, m_p, m_p2;
    
  };

  Detector *detector = new Detector("detector");

  double x,y,z, vx,vy,vz, t, s1, s2, p = 1.;
  detector->trace( x,y,z, vx,vy,vz, t, s1,s2, p );

  delete detector;

  std::cout << ">> Test of 'DETECTOR_OUT_1D' passed" << std::endl;
  return 0;
}


int main()
{
  test_0D();
  test_1D();
  return 0;
}
