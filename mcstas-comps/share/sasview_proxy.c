  #if model_index == 1
    %include "sas_barbell.c"
  #endif
  #if model_index == 2
    %include "sas_barbell.c"
  #endif
  #if model_index == 3
    %include "sas_bcc_paracrystal.c"
  #endif
  #if model_index == 4
    %include "sas_bcc_paracrystal.c"
  #endif
  #if model_index == 5
    %include "sas_capped_cylinder.c"
  #endif
  #if model_index == 6
    %include "sas_capped_cylinder.c"
  #endif
  #if model_index == 7
    %include "sas_core_shell_cylinder.c"
  #endif
  #if model_index == 8
    %include "sas_core_shell_cylinder.c"
  #endif
  #if model_index == 9
    %include "sas_cylinder.c"
  #endif
  #if model_index == 10
    %include "sas_cylinder.c"
  #endif
  #if model_index == 11
    %include "sas_dab.c"
  #endif
  #if model_index == 12
    %include "sas_dab.c"
  #endif
  #if model_index == 13
    %include "sas_ellipsoid.c"
  #endif
  #if model_index == 14
    %include "sas_ellipsoid.c"
  #endif
  #if model_index == 15
    %include "sas_fcc_paracrystal.c"
  #endif
  #if model_index == 16
    %include "sas_fcc_paracrystal.c"
  #endif
  #if model_index == 17
    %include "sas_gaussian_peak.c"
  #endif
  #if model_index == 18
    %include "sas_gaussian_peak.c"
  #endif
  #if model_index == 19
    %include "sas_hardsphere.c"
  #endif
  #if model_index == 20
    %include "sas_hardsphere.c"
  #endif
  #if model_index == 21
    %include "sas_HayterMSAsq.c"
  #endif
  #if model_index == 22
    %include "sas_HayterMSAsq.c"
  #endif
  #if model_index == 23
    %include "sas_lamellar.c"
  #endif
  #if model_index == 24
    %include "sas_lamellar.c"
  #endif
  #if model_index == 25
    %include "sas_lamellarCailleHG.c"
  #endif
  #if model_index == 26
    %include "sas_lamellarCailleHG.c"
  #endif
  #if model_index == 27
    %include "sas_lamellarPC.c"
  #endif
  #if model_index == 28
    %include "sas_lamellarPC.c"
  #endif
  #if model_index == 29
    %include "sas_parallelepiped.c"
  #endif
  #if model_index == 30
    %include "sas_parallelepiped.c"
  #endif
  #if model_index == 31
    %include "sas_sphere.c"
  #endif
  #if model_index == 32
    %include "sas_sphere.c"
  #endif
  #if model_index == 33
    %include "sas_stickyhardsphere.c"
  #endif
  #if model_index == 34
    %include "sas_stickyhardsphere.c"
  #endif
  #if model_index == 35
    %include "sas_triaxial_ellipsoid.c"
  #endif
  #if model_index == 36
    %include "sas_triaxial_ellipsoid.c"
  #endif

  float getIq(float q, float qx, float qy, float pars[])
  {
    float Iq_out = 1;
    #if model_index == 1
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 2
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6]);
    #endif
    #if model_index == 3
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 4
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if model_index == 5
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 6
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6]);
    #endif
    #if model_index == 7
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if model_index == 8
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if model_index == 9
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if model_index == 10
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if model_index == 11
      Iq_out = Iq(q, pars[0]);
    #endif
    #if model_index == 12
      Iq_out = Iqxy(qx, qy, pars[0]);
    #endif
    #if model_index == 13
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if model_index == 14
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if model_index == 15
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 16
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if model_index == 17
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if model_index == 18
      Iq_out = Iqxy(qx, qy, pars[0], pars[1]);
    #endif
    #if model_index == 19
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if model_index == 20
      Iq_out = Iqxy(qx, qy, pars[0], pars[1]);
    #endif
    #if model_index == 21
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if model_index == 22
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if model_index == 23
      Iq_out = Iq(q, pars[0], pars[1], pars[2]);
    #endif
    #if model_index == 24
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2]);
    #endif
    #if model_index == 25
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if model_index == 26
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if model_index == 27
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if model_index == 28
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if model_index == 29
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 30
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if model_index == 31
      Iq_out = Iq(q, pars[0], pars[1], pars[2]);
    #endif
    #if model_index == 32
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2]);
    #endif
    #if model_index == 33
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if model_index == 34
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if model_index == 35
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 36
      Iq_out = Iqxy(qx, qy, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    return Iq_out;
  }

  float getFormVol(float pars[])
  {
    float form_vol;
    #if model_index == 1
      form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 2
      form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 3
      form_vol = form_volume(pars[2]);
    #endif
    #if model_index == 4
      form_vol = form_volume(pars[2]);
    #endif
    #if model_index == 5
      form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 6
      form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 7
      form_vol = form_volume(pars[3], pars[4], pars[5]);
    #endif
    #if model_index == 8
      form_vol = form_volume(pars[3], pars[4], pars[5]);
    #endif
    #if model_index == 9
      form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if model_index == 10
      form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if model_index == 11
      form_vol = 1;
    #endif
    #if model_index == 12
      form_vol = 1;
    #endif
    #if model_index == 13
      form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if model_index == 14
      form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if model_index == 15
      form_vol = form_volume(pars[2]);
    #endif
    #if model_index == 16
      form_vol = form_volume(pars[2]);
    #endif
    #if model_index == 17
      form_vol = form_volume();
    #endif
    #if model_index == 18
      form_vol = form_volume();
    #endif
    #if model_index == 19
      form_vol = form_volume(pars[0]);
    #endif
    #if model_index == 20
      form_vol = form_volume(pars[0]);
    #endif
    #if model_index == 21
      form_vol = form_volume(pars[0]);
    #endif
    #if model_index == 22
      form_vol = form_volume(pars[0]);
    #endif
    #if model_index == 23
      form_vol = form_volume(pars[2]);
    #endif
    #if model_index == 24
      form_vol = form_volume(pars[2]);
    #endif
    #if model_index == 25
      form_vol = 1;
    #endif
    #if model_index == 26
      form_vol = 1;
    #endif
    #if model_index == 27
      form_vol = 1;
    #endif
    #if model_index == 28
      form_vol = 1;
    #endif
    #if model_index == 29
      form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 30
      form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 31
      form_vol = form_volume(pars[2]);
    #endif
    #if model_index == 32
      form_vol = form_volume(pars[2]);
    #endif
    #if model_index == 33
      form_vol = form_volume(pars[0]);
    #endif
    #if model_index == 34
      form_vol = form_volume(pars[0]);
    #endif
    #if model_index == 35
      form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if model_index == 36
      form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    return form_vol;
  }
