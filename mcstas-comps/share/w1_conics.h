/**
    @file w1_conics.h
    \brief Functions for generating and nesting shell surfaces specific to neutron wolter1 mirrors. 
    @author Giacomo Resta <gresta@mit.edu>
    @version 0.1-beta

    @section LICENSE

    Permission is hereby granted, free of charge, to any person obtaining a 
    copy of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense, 
    and/or sell copies of the Software, and to permit persons to whom the Software
    is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
    INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
    PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
    FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
    OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.

    @section DESCRIPTION
    Functions for generating and nesting shell surfaces specific to 
    neutron wolter1 mirrors.
*/

#ifndef MIT_W1_CONICS
#define MIT_W1_CONICS

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "conic.h"

///////////////////////////////////
// PP
///////////////////////////////////

/** @defgroup ppgroup PP
    Contains items for PP shells
     @{
*/

//! Structure to represent PP geometry
/*! Defines two paraboloid with wider ends facing each other. 

    @warning: Do not modify items in this structure directly */
typedef struct {
    ConicSurf* p0; //!< Pointer to Paraboloid closest to first focus
    ConicSurf* p1; //!< Pointer to Paraboloid closest to second focus 
    Disk* ds0; //!< Disk to simulate thickness of p0
    Disk* de0; //!< Second Disk to simulate thickness of p0
    Disk* ds1; //!< Disk to simulate thickness of p1
    Disk* de1; //!< Second Disk to simulate thickness of p1
    
    //For reference only
    double mirr_thick; //!< Thickness of mirrors               
    double fp; //!< distance from first focus to end of p0
    double M; //!< Magnification of mirrors
} PP;

/*! \brief Function to add PP Shells

@param f1 z position of focus closest to origin
@param f2 z position of focus farthest from origin
@param r largest radius of the paraboloid
@param fp distance from origin to end of first paraboloid (p0)
@param M Magnification of mirrors
@param lp1 length of first paraboloid (p1) second paraboloid is auto calculated
@param m1 m reflection value for first paraboloid
@param m2 m reflection value for second paraboloid
@param mirr_thick thickness of the shells
@param s Pointer of Scene to add Items to
*/
PP addPPShell(double f1, double f2, double r, double fp, double M, double lp1,
        double m1, double m2, double mirr_thick, Scene* s) {
    PP p;

    //Add the two Conic Shells
    double fp2 = fp*M;
    p.p0 = addParaboloid(f1,makePoint(0,r,f1+fp), f1+fp-lp1, f1+fp, m1, s);

    //Add second Paraboloid Intelligently
    ConicSurf ptest = makeParaboloid(f2, makePoint(0,r,f2-fp2), f2-fp2, f2-fp2+1, m2);
    double zs = (pow(rConic(p.p0->ze, *p.p0),2)-ptest.k1)/ptest.k2; 
    double ze = (pow(rConic(p.p0->zs, *p.p0),2)-ptest.k1)/ptest.k2;  
    
    p.p1 = addParaboloid(f2, makePoint(0,r,f2-fp2), zs, ze, m2, s);
    
    //Add the Disks to simulate thickness
    p.mirr_thick = mirr_thick;
    p.ds0 = addDisk(p.p0->zs, rConic(p.p0->zs, *p.p0), rConic(p.p0->zs, *p.p0)+mirr_thick,s);
    p.de0 = addDisk(p.p0->ze, rConic(p.p0->ze, *p.p0), rConic(p.p0->ze, *p.p0)+mirr_thick,s);
    p.ds1 = addDisk(p.p1->zs, rConic(p.p1->zs, *p.p1), rConic(p.p1->zs, *p.p1)+mirr_thick,s);
    p.de1 = addDisk(p.p1->ze, rConic(p.p1->ze, *p.p1), rConic(p.p1->ze, *p.p1)+mirr_thick,s);

    p.fp = fp;
    p.M = M;
     
    return p;
}

/*! \brief Function to nest PP Shell under another PP Shell

@param p PP Shell to nest under
@param s Pointer of Scene to add Items to
*/
PP addPrevPPShell(PP p, Scene* s) {
    //Test Case One
    double re = rConic(p.p0->zs, *p.p0) - p.mirr_thick;

    //Test Case Two
    double rs2 = rConic(p.p0->ze, *p.p0)*p.p0->zs/p.p0->ze - p.mirr_thick;
    ConicSurf ptest = makeParaboloid(p.p0->f1, makePoint(0,rs2,p.p0->zs), p.p0->zs,
                p.p0->ze, p.p0->m);
    double re2 = rConic(ptest.ze, ptest);

    //Choose Smallest End Radius
    if (re > re2)
        re = re2;

    return addPPShell(p.p0->f1, p.p1->f1, re, p.fp, p.M,
            p.p0->ze-p.p0->zs, p.p0->m, p.p1->m, p.mirr_thick, s);
}

/** @} */ //end ppgroup

///////////////////////////////////      
// HPPH
/////////////////////////////////// 

/** @defgroup hpphgroup HPPH
    Contains items for HPPH shells
     @{
*/  

//! Structure to represent HPPH geometry
/*! Defines two hyperboloid-paraboloid surfaces with wider ends facing each other.
    @warning Do not modify items in struct directly.Strange things may happen with M != 1*/
typedef struct {
    ConicSurf* h0; //!< Pointer to Hyperboloid closest to first focus
    ConicSurf* p0; //!< Pointer to Paraboloid closest to first focus
    ConicSurf* p1; //!< Pointer to Paraboloid closest to second focus
    ConicSurf* h1; //!< Pointer to Hyperboloid closest to second focus
    Disk* d0; //!< Pointer to Disk to simulate thickness of h0
    Disk* d1; //!< Pointer to Disk to simulate thickness of p1

    //For reference only
    double mirr_thick; //!< Thickness of mirrors
    double fh; //!< length from first focus to end of first hyperboloid (h0)
    double M; //!< Magnification of mirrors
} HPPH;

/*! \brief Function to add HPPH Shells

@param f1 z position of focus closest to origin
@param f2 z position of focus farthest from origin
@param r largest radius of the hyperboloids
@param fh distance from origin to end of first hyperboloid (h0)
@param M Magnification of mirrors
@param lhp1 length of first hyperboloid paraboloid surface (h0+p0) second HP Surface is auto calculated
@param m1 m reflection value for first HP Surface (h0+p0)
@param m2 m reflection value for second HP Surface (h0+p0)
@param mirr_thick thickness of the shells
@param s Pointer of Scene to add Items to
*/       
HPPH addHPPHShell(double f1, double f2, double r, double fh, double M, double lhp1, double m1, 
        double m2, double mirr_thick, Scene* s) {

    HPPH p;

    if (M != 1)
        printf("EQUATIONS FOR HPPH SHELLS DO NOT WORK CORRECTLY FOR M != 1\n");

    double fp1 = 2*r/atan(r/fh);
    double lh1 = lhp1/2;
    double lp1 = lhp1;

    ConicSurf htest = makeHyperboloid(f1,f1+fh-fp1, makePoint(0,r,f1+fh), f1+fh-lh1, f1+fh, m1);
    ConicSurf ptest = makeParaboloid(f1+fh-fp1, makePoint(0,r,f1+fh), f1+fh, f1+fh+lp1,m1);
       
    do {
        lh1 -= 0.0001;
        double phi = atan(rConic(f1+fh-lh1, htest)/(fabs(fp1)-lh1));
        lp1 = 2*ptest.a*cos(phi)/(1-cos(phi))-fabs(fp1);

    } while (lh1+lp1 > lhp1);

    //Add Conic Shells
    p.h0 = addHyperboloid(f1,f1+fh-fp1, makePoint(0,r,f1+fh), f1+fh-lh1, f1+fh, m1,s);
    p.p0 = addParaboloid(f1+fh-fp1, makePoint(0,r,f1+fh), f1+fh, f1+fh+lp1,m1,s);

    double fh2 = fh*M;
    double fp2 = 2*r/atan(r/fh2); 
    
    //Add second Paraboloid Intelligently using zs and ze radius for first paraboloid  
    ptest = makeParaboloid(f2+fp2-fh2, makePoint(0,r,f2-fh2), f2-fp2, f2-fp2+1, m2);
    htest = makeHyperboloid(f2,f2+fp2-fh2, makePoint(0,r,f2-fh2), f2-fp2, f2-fp2+1, m2);

    double zs = (pow(rConic(p.p0->ze, *p.p0),2)-ptest.k1)/ptest.k2; 
    double ze = (pow(rConic(p.p0->zs, *p.p0),2)-ptest.k1)/ptest.k2;  
    
    //Compute ideal length of h1 based on p1
    double phi = atan(rConic(zs, ptest)/(f2+fp2-fh2-zs));
    double rh_zeh = (htest.c*htest.c-htest.a*htest.a)/(htest.a-htest.c*cos(phi));
    double zeh = f2+fp2-fh2-rh_zeh/tan(phi);
    double zeh2 = lhp1+zs;
    
    if (zeh2 < zeh)
        zeh = zeh2;

    //Add Conic Shells
    p.p1 = addParaboloid(f2+fp2-fh2, makePoint(0,r,f2-fh2), zs, ze, m2,s);
    p.h1 = addHyperboloid(f2, f2+fp2-fh2, makePoint(0,r,f2-fh2), ze, zeh, m2,s);

    //Add Disks to simulate thickness
    p.mirr_thick = mirr_thick;
    p.d0 = addDisk(p.h0->zs, rConic(p.h0->zs, *p.h0), rConic(p.h0->zs, *p.h0)+mirr_thick,s);
    p.d1 = addDisk(p.p1->zs, rConic(p.p1->zs, *p.p1), rConic(p.p1->zs, *p.p1)+mirr_thick,s);
    
    p.fh = fh;
    p.M = M;

    return p;
}

/*! \brief Function to nest HPPH Shell on top of another HPPH Shell

@param p HPPH Shell to nest on top of
@param s Pointer of Scene to add Items to
*/
HPPH addNextHPPHShell(HPPH p, Scene* s) {
    double r = (rConic(p.h0->zs, *p.h0)+p.mirr_thick)*p.h0->ze/p.h0->zs;
    return addHPPHShell(p.h0->f1, p.h1->f1, r, p.fh, p.M, 
            p.p0->ze-p.h0->zs, p.h0->m, p.h1->m, p.mirr_thick,s);
}
 
/** @} */ //end of hpphgroup

///////////////////////////////////
// EH
/////////////////////////////////// 

/** @defgroup ehgroup EH
    Contains items for EH shells
     @{
*/  

//! Structure to represent Ellipsoid-Hyperboloid Shell
typedef struct {
    ConicSurf* e; //!< Pointer to Ellipsoid
    ConicSurf* h; //!< Pointer to Hyperboloid
    Disk* d0;  //!< Pointer to Disk to simulate Ellipsoid thickness
    Disk* d1;  //!< Pointer to Disk to simulate Hyperboloid thickness
} EH;

/*! \brief  Function to add Ellipsoid-Hyperboloid Shell to Scene

@param f1 z-position of source (first focus)
@param f2 z-position of second focus
@param r radius at intersection between Ellipsoid and Hyperboloid
@param M Magnification of Shell
@param le Length of Ellipsoid
@param lh Length of Hyperboloid
@param m Reflectivity
@param mirr_thick Thickness of mirror shells
@param s Scene
*/
EH addEHShell(double f1, double f2, double r, double M, double le, double lh, double m,
        double mirr_thick, Scene* s) {
    EH p;

    double f = (f2-f1)/(1+M);
    double fh = f*M;
    double th = (atan(r/f)+atan(r/fh))/4;
    double feh = fabs(fh*th/(r/f-2*th))*2;    //Remove *2 to increase performance by 20%

    p.e = addEllipsoid(f1,f2+feh,makePoint(0,r,f+f1),f1+f-le,f1+f,m,s);
    p.h = addHyperboloid(f2,f2+feh, makePoint(0,r,f+f1), f1+f, f1+f+lh, m,s);

    p.d0 = addDisk(p.e->zs, rConic(p.e->zs, *p.e), rConic(p.e->zs, *p.e)+mirr_thick,s);
    p.d1 = addDisk(p.h->ze, rConic(p.h->ze, *p.h), rConic(p.h->ze, *p.h)+mirr_thick,s);
    
    return p;
}


/*! \brief Function to add Ellipsoid-Hyperboloid Shell to Scene using Ah, Bh, Ae, Be, and 
position of intersection.

@param f1 z-position of source (first focus)
@param f2 z-position of second focus
@param r Radius at intersection between Ellipsoid and Hyperboloid
@param zr z-position of intersection between Ellipsoid and Hyperboloid 
@param le Length of Ellipsoid
@param lh Length of Hyperboloid
@param m Reflectivity
@param mirr_thick Thickness of mirror shells
@param Ae Value of a for Ellipsoid
@param Be Value of b for Ellipsoid
@param Ah Value of a for Hyperboloid
@param Bh Value of b for Hyperboloid
@param s Pointer to Scene 
*/

EH addEHShellM(double f1, double f2, double r, double zr, double le, double lh, double m,
        double mirr_thick, double Ae, double Be, double Ah, double Bh, Scene* s) {
    EH p;

    double fh = sqrt(Ah*Ah+Bh*Bh)*2;
    double fe = sqrt(Ae*Ae-Be*Be)*2;

    p.e = addEllipsoid(f1,f1+fe,makePoint(0,r,zr+f1),f1+zr-le,f1+zr,m,s);
    p.h = addHyperboloid(f2,f2+fh, makePoint(0,r,zr+f1), f1+zr, f1+zr+lh, m,s);

    p.d0 = addDisk(p.e->zs, rConic(p.e->zs, *p.e), rConic(p.e->zs, *p.e)+mirr_thick,s);
    p.d1 = addDisk(p.h->ze, rConic(p.h->ze, *p.h), rConic(p.h->ze, *p.h)+mirr_thick,s);
    
    return p;
}

EH addHEShellM(double f1, double f2, double r, double zr, double le, double lh, double m,
               double mirr_thick, double Ae, double Be, double Ah, double Bh, Scene* s) {
    EH p;
    
    double fh = sqrt(Ah*Ah+Bh*Bh)*2;
    double fe = sqrt(Ae*Ae-Be*Be)*2;
    
    zr=f2-zr;
    
    p.e = addEllipsoid(f2,f2-fe,makePoint(0,r,zr+f1),f1+zr, f1+zr+le, m,s);
    p.h = addHyperboloid(f1,f1-fh, makePoint(0,r,zr+f1), f1+zr-lh, f1+zr, m,s);
    
    p.d0 = addDisk(p.e->ze, rConic(p.e->ze, *p.e), rConic(p.e->ze, *p.e)+mirr_thick,s);
    p.d1 = addDisk(p.h->zs, rConic(p.h->zs, *p.h), rConic(p.h->zs, *p.h)+mirr_thick,s);
    
    return p;
}

//!TODO
EH addNextEHShell(EH p) {
    //
}
  
/** @} */ //end of ehgroup

///////////////////////////////////
// HPPH TOP - PP BOTTOM Interface 
/////////////////////////////////// 

/*! \ingroup ppgroup
    \brief Function to make first PP Shell under smallest HPPH Shell

    @param p HPPH Shell to nest PP under
    @param s Pointer of Scene to add Items to
*/
PP addFirstPPShell(HPPH p, Scene* s) {
    return addPPShell(p.h0->f1, p.h1->f1, rConic(p.h0->ze, *p.h0)*p.h0->zs/p.h0->ze, p.p0->ze-p.h0->f1,
            p.M, p.p0->ze-p.h0->zs, p.h0->m, p.h1->m, p.mirr_thick, s);
}        

///////////////////////////////////
/////////////////////////////////// 

#endif
