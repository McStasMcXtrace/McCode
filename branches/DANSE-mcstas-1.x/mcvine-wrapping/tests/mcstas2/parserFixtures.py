# -*- Python -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                               Alex Dementsov
#                      California Institute of Technology
#                        (C) 2010  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#

testtext = """
/*******************************************************************************
*
*
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Component: E_monitor
*
* %I
* Written by: Kristian Nielsen and Kim Lefmann
* Date: April 20, 1998
* Version: $Revision: 438 $
* Origin: Risoe
* Release: McStas 1.6
*
* Energy-sensitive monitor.
*
* %D
* A square single monitor that measures the energy of the incoming neutrons.
*
* Example: E_monitor(xmin=-0.1, xmax=0.1, ymin=-0.1, ymax=0.1,
*                 Emin=1, Emax=50, nchan=20, filename="Output.nrj")
*
* %P
* INPUT PARAMETERS:
*
* xmin:     Lower x bound of detector opening (m)
* xmax:     Upper x bound of detector opening (m)
* ymin:     Lower y bound of detector opening (m)
* ymax:     Upper y bound of detector opening (m)
* Emin:     Minimum energy to detect (meV)
* Emax:     Maximum energy to detect (meV)
* nchan:    Number of energy channels (1)
* filename: Name of file in which to store the detector image (text)
*
* OUTPUT PARAMETERS:
*
* E_N:      Array of neutron counts
* E_p:      Array of neutron weight counts
* E_p2:     Array of second moments
*
* %E
*******************************************************************************/
the rest of text
"""

snstext = """
DEFINE COMPONENT SNS_source
DEFINITION PARAMETERS ()
SETTING PARAMETERS (char *S_filename="SNS_moderator_data_file",width=0.1, height=0.12, dist=2.5, xw=0.1, yh=0.12, Emin=50, Emax=70)
OUTPUT PARAMETERS (hdiv,vdiv,p_in)
STATE PARAMETERS (x,y,z,vx,vy,vz,t,s1,s2,p)
"""


# XXX: Check if split by lines for definitions is legal
psd_tew = """
DEFINE COMPONENT PSD_TEW_monitor
DEFINITION PARAMETERS (nxchan=20, nychan=20, nbchan=20, string type="time", string filename, string format="table")
SETTING PARAMETERS (xwidth=0, yheight=0, bmin=0, bmax=0, deltab=0,
                    restore_neutron=0)
OUTPUT PARAMETERS (TOF_N, TOF_p, TOF_p2, b_min, b_max, delta_b, x_min, x_max, delta_x, y_min, y_max, delta_y)
STATE PARAMETERS (x,y,z,vx,vy,vz,t,s1,s2,p)
POLARISATION PARAMETERS (sx,sy,sz)
"""

iqetext = """
DEFINE COMPONENT IQE_monitor
DEFINITION PARAMETERS ()
SETTING PARAMETERS (Ei=60, Qmin=0, Qmax=10, Emin=-45, Emax=45, int nQ=100,
int nE=90, max_angle_in_plane = 120, min_angle_in_plane = 0,
max_angle_out_of_plane = 30, min_angle_out_of_plane = -30, char *filename = "iqe_monitor.dat")
OUTPUT PARAMETERS () //(IQE_N, IQE_p, IQE_p2)
STATE PARAMETERS (x,y,z,vx,vy,vz,t,s1,s2,p)
"""

sourcegen = """
DEFINE COMPONENT Source_gen
DEFINITION PARAMETERS (string flux_file=0, string xdiv_file=0, string ydiv_file=0)
SETTING PARAMETERS (radius=0.0, dist=0, xw=0, yh=0, E0=0, dE=0, Lambda0=0, dLambda=0, I1=0,
                    h=0, w=0, verbose=0, T1=0,
                    flux_file_perAA=0, flux_file_log=0,
                    Lmin=0,Lmax=0,Emin=0,Emax=0,T2=0,I2=0,T3=0,I3=0,length=0)
OUTPUT PARAMETERS (p_in, lambda0, lambda02, L2P, lambda0b, lambda02b, L2Pb,lambda0c, lambda02c, L2Pc, pTable, pTable_x, pTable_y,pTable_xmin, pTable_xmax, pTable_xsum, pTable_ymin, pTable_ymax, pTable_ysum, pTable_dxmin, pTable_dxmax, pTable_dymin, pTable_dymax)
STATE PARAMETERS (x,y,z,vx,vy,vz,t,s1,s2,p)
"""

__date__ = "$Sep 15, 2010 3:17:26 PM$"


