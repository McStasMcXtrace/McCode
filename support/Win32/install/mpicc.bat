@rem    This file is part of the McStas neutron ray-trace simulation package
@rem    Copyright (C) 1997-2004, All rights reserved
@rem    Risoe National Laborartory, Roskilde, Denmark
@rem    Institut Laue Langevin, Grenoble, France
@rem
@rem    This program is free software; you can redistribute it and/or modify
@rem    it under the terms of the GNU General Public License as published by
@rem    the Free Software Foundation; version 3 of the License.
@rem
@rem    This program is distributed in the hope that it will be useful,
@rem    but WITHOUT ANY WARRANTY; without even the implied warranty of
@rem    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@rem    GNU General Public License for more details.
@rem
@rem    You should have received a copy of the GNU General Public License
@rem    along with this program; if not, write to the Free Software
@rem    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  
@rem    USA
@rem    
@rem    Batch for calling gcc with MPI options on Win32
@rem
@rem    Configuration:
@rem
@rem    Set -I and -L according to your installation of the MPICH2 libs
@rem    WARNING: The dev-cpp gcc compiler DISLIKES drive specification, e.g
@rem    -Ic:/someting will NOT work

gcc %*% -I/progra~1/mpich2/include -L/progra~1/mpich2/lib -lmpi
