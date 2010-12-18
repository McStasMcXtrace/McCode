// -*- C++ -*-

#ifndef H_MCSTAS_compact_INTERSECT
#define H_MCSTAS_compact_INTERSECT


namespace McStas {
  int box_intersect(double *dt_in, double *dt_out, double x, double y, double z,
		    double vx, double vy, double vz, double dx, double dy, double dz);
  int cylinder_intersect(double *t0, double *t1, double x, double y, double z,
			 double vx, double vy, double vz, double r, double h);
  int sphere_intersect(double *t0, double *t1, double x, double y, double z,
		       double vx, double vy, double vz, double r);
} //McStas


#endif // H_MCSTAS_compact_INTERSECT
