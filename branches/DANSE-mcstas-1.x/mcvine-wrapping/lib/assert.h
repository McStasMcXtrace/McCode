// -*- C++ -*-

#ifndef H_MCSTAS2_ASSERT
#define H_MCSTAS2_ASSERT


#include "exception.h"
#include "abs.h"


// assertion functions. useful for tests


namespace mcstas2{


  /// assertion exception class
  class AssertionError : public Exception {
    
  public:
    AssertionError( const char * msg = "assertion error" ) 
      : Exception( msg ) {}
  };

  

  /// assert two numbers are equal. only good for integers
  template <class Num1, class Num2>
  void assertNumberEqual( Num1 a, Num2 b );
  

  /// assert two numbers are almost equal. good for floats
  /*!
    @param a,b: two input numbers
    @param relerr: if the relative difference between the two input numbers is 
    larger than "relerr', then we say the two numbers are not almost equal
    @param abserr: if the absolute difference between the two input numbers is
    smaller than 'abserr', then we say the two numbers are not almost equal
   */
  template <class Num1, class Num2>
  void assertNumberAlmostEqual
  ( Num1 a, Num2 b, double relerr=0.01, double abserr = 1e-10 );


  /// assert two vectors are almost equal
  /*!
    @param v1, v2: two input vectors
    @param relerr, abserr: refer to function assertNumberAlmostEqual
   */
  template <class Vector1, class Vector2>
  void assertVectorAlmostEqual
  ( const Vector1 & v1, const Vector2 & v2, 
    double relerr=0.01, double abserr=1e-30);
  

  /// assert two vectors are almost equal
  /*!
    the "double" version of assertNumberAlmostEqual. It is expected to be
    used most frequently, so we give it a special name
   */
  inline void assertAlmostEqual
  ( double a, double b, double relerr=0.01, double abserr = 1e-10) {
    assertNumberAlmostEqual( a, b, relerr, abserr );
  }

} // mcstas2

#include <iostream>
#include <sstream>


// implementations


template <class Num1, class Num2>
void mcstas2::assertNumberEqual( Num1 a, Num2 b)
{
  std::ostringstream oss;

  if (a!=b) {
    oss << a << " is not equal to " << b;
    throw AssertionError( oss.str().c_str() );
  }
}


namespace mcstas2 { namespace Assert {namespace impl_details{

  template <class Type> 
  std::string not_almost_equal_err_msg( const Type & a, const Type & b ) {

    std::ostringstream oss;
    oss << a << " is not almost equal to " << b;
    return oss.str();

  }

}}}


template <class Num1, class Num2>
void mcstas2::assertNumberAlmostEqual
( Num1 a, Num2 b, double relerr, double abserr)
{
  using mcstas2::abs;
  using Assert::impl_details::not_almost_equal_err_msg;
  
  // if both inputs are very close to zero, then we say we are good
  double bigger = std::max( abs(a), abs(b) );
  if (bigger <= abserr) return;

  // otherwise we first find out if the absolute difference meets the criteria
  double absdiff = abs(a-b);
  if (absdiff > abserr) throw AssertionError( not_almost_equal_err_msg( a,b ).c_str() );
  
  // if we reach here, we should check the relative difference
  double reldiff = absdiff/bigger;
  if (reldiff > relerr) throw AssertionError( not_almost_equal_err_msg( a,b ).c_str() );
}


template <class Vector1, class Vector2>
void mcstas2::assertVectorAlmostEqual( const Vector1 & v1, const Vector2 & v2, double relerr, double abserr )
{
  if (v1.size() != v2.size()) {
    throw AssertionError( "vector has difference sizes" );
  }
  for (int i=0; i<v1.size(); i++) {
    assertNumberAlmostEqual( v1[i], v2[i], relerr, abserr );
  }
}


#endif // H_MCSTAS2_ASSERT
