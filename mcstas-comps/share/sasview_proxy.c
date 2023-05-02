#define FLOAT_SIZE 8
#if SASmodel_index == 1
    // lorentz : Iq
    %include "sas_lorentz.c"
  #endif
#if SASmodel_index == 2
    // ellipsoid : Fq
    %include "sas_ellipsoid.c"
  #endif
#if SASmodel_index == 3
    // ellipsoid : Iqac
    %include "sas_ellipsoid.c"
  #endif
#if SASmodel_index == 4
    // spinodal : Iq
    %include "sas_spinodal.c"
  #endif
#if SASmodel_index == 5
    // lamellar_hg_stack_caille : Iq
    %include "sas_lamellar_hg_stack_caille.c"
  #endif
#if SASmodel_index == 6
    // fractal_core_shell : Iq
    %include "sas_fractal_core_shell.c"
  #endif
#if SASmodel_index == 7
    // dab : Iq
    %include "sas_dab.c"
  #endif
#if SASmodel_index == 8
    // peak_lorentz : Iq
    %include "sas_peak_lorentz.c"
  #endif
#if SASmodel_index == 9
    // core_shell_bicelle_elliptical_belt_rough : Fq
    %include "sas_core_shell_bicelle_elliptical_belt_rough.c"
  #endif
#if SASmodel_index == 10
    // core_shell_bicelle_elliptical_belt_rough : Iqabc
    %include "sas_core_shell_bicelle_elliptical_belt_rough.c"
  #endif
#if SASmodel_index == 11
    // binary_hard_sphere : Iq
    %include "sas_binary_hard_sphere.c"
  #endif
#if SASmodel_index == 12
    // correlation_length : Iq
    %include "sas_correlation_length.c"
  #endif
#if SASmodel_index == 13
    // hollow_rectangular_prism_thin_walls : Fq
    %include "sas_hollow_rectangular_prism_thin_walls.c"
  #endif
#if SASmodel_index == 14
    // superball : Fq
    %include "sas_superball.c"
  #endif
#if SASmodel_index == 15
    // superball : Iqabc
    %include "sas_superball.c"
  #endif
#if SASmodel_index == 16
    // gaussian_peak : Iq
    %include "sas_gaussian_peak.c"
  #endif
#if SASmodel_index == 17
    // vesicle : Fq
    %include "sas_vesicle.c"
  #endif
#if SASmodel_index == 18
    // flexible_cylinder_elliptical : Iq
    %include "sas_flexible_cylinder_elliptical.c"
  #endif
#if SASmodel_index == 19
    // lamellar_stack_caille : Iq
    %include "sas_lamellar_stack_caille.c"
  #endif
#if SASmodel_index == 20
    // guinier_porod : Iq
    %include "sas_guinier_porod.c"
  #endif
#if SASmodel_index == 21
    // spherical_sld : Iq
    %include "sas_spherical_sld.c"
  #endif
#if SASmodel_index == 22
    // capped_cylinder : Fq
    %include "sas_capped_cylinder.c"
  #endif
#if SASmodel_index == 23
    // capped_cylinder : Iqac
    %include "sas_capped_cylinder.c"
  #endif
#if SASmodel_index == 24
    // adsorbed_layer : Iq
    %include "sas_adsorbed_layer.c"
  #endif
#if SASmodel_index == 25
    // core_multi_shell : Fq
    %include "sas_core_multi_shell.c"
  #endif
#if SASmodel_index == 26
    // poly_gauss_coil : Iq
    %include "sas_poly_gauss_coil.c"
  #endif
#if SASmodel_index == 27
    // line : Iq
    %include "sas_line.c"
  #endif
#if SASmodel_index == 28
    // stacked_disks : Iq
    %include "sas_stacked_disks.c"
  #endif
#if SASmodel_index == 29
    // stacked_disks : Iqac
    %include "sas_stacked_disks.c"
  #endif
#if SASmodel_index == 30
    // porod : Iq
    %include "sas_porod.c"
  #endif
#if SASmodel_index == 31
    // hardsphere : Iq
    %include "sas_hardsphere.c"
  #endif
#if SASmodel_index == 32
    // core_shell_bicelle : Fq
    %include "sas_core_shell_bicelle.c"
  #endif
#if SASmodel_index == 33
    // core_shell_bicelle : Iqac
    %include "sas_core_shell_bicelle.c"
  #endif
#if SASmodel_index == 34
    // cylinder : Fq
    %include "sas_cylinder.c"
  #endif
#if SASmodel_index == 35
    // cylinder : Iqac
    %include "sas_cylinder.c"
  #endif
#if SASmodel_index == 36
    // polymer_excl_volume : Iq
    %include "sas_polymer_excl_volume.c"
  #endif
#if SASmodel_index == 37
    // fcc_paracrystal : Iq
    %include "sas_fcc_paracrystal.c"
  #endif
#if SASmodel_index == 38
    // fcc_paracrystal : Iqabc
    %include "sas_fcc_paracrystal.c"
  #endif
#if SASmodel_index == 39
    // fractal : Iq
    %include "sas_fractal.c"
  #endif
#if SASmodel_index == 40
    // power_law : Iq
    %include "sas_power_law.c"
  #endif
#if SASmodel_index == 41
    // guinier : Iq
    %include "sas_guinier.c"
  #endif
#if SASmodel_index == 42
    // two_lorentzian : Iq
    %include "sas_two_lorentzian.c"
  #endif
#if SASmodel_index == 43
    // stickyhardsphere : Iq
    %include "sas_stickyhardsphere.c"
  #endif
#if SASmodel_index == 44
    // gauss_lorentz_gel : Iq
    %include "sas_gauss_lorentz_gel.c"
  #endif
#if SASmodel_index == 45
    // mass_surface_fractal : Iq
    %include "sas_mass_surface_fractal.c"
  #endif
#if SASmodel_index == 46
    // polymer_micelle : Iq
    %include "sas_polymer_micelle.c"
  #endif
#if SASmodel_index == 47
    // flexible_cylinder : Iq
    %include "sas_flexible_cylinder.c"
  #endif
#if SASmodel_index == 48
    // parallelepiped : Fq
    %include "sas_parallelepiped.c"
  #endif
#if SASmodel_index == 49
    // parallelepiped : Iqabc
    %include "sas_parallelepiped.c"
  #endif
#if SASmodel_index == 50
    // hollow_rectangular_prism : Fq
    %include "sas_hollow_rectangular_prism.c"
  #endif
#if SASmodel_index == 51
    // hollow_rectangular_prism : Iqabc
    %include "sas_hollow_rectangular_prism.c"
  #endif
#if SASmodel_index == 52
    // rectangular_prism : Iq
    %include "sas_rectangular_prism.c"
  #endif
#if SASmodel_index == 53
    // rectangular_prism : Iqabc
    %include "sas_rectangular_prism.c"
  #endif
#if SASmodel_index == 54
    // core_shell_parallelepiped : Fq
    %include "sas_core_shell_parallelepiped.c"
  #endif
#if SASmodel_index == 55
    // core_shell_parallelepiped : Iqabc
    %include "sas_core_shell_parallelepiped.c"
  #endif
#if SASmodel_index == 56
    // lamellar_hg : Iq
    %include "sas_lamellar_hg.c"
  #endif
#if SASmodel_index == 57
    // triaxial_ellipsoid : Fq
    %include "sas_triaxial_ellipsoid.c"
  #endif
#if SASmodel_index == 58
    // triaxial_ellipsoid : Iqabc
    %include "sas_triaxial_ellipsoid.c"
  #endif
#if SASmodel_index == 59
    // sc_paracrystal : Iq
    %include "sas_sc_paracrystal.c"
  #endif
#if SASmodel_index == 60
    // sc_paracrystal : Iqabc
    %include "sas_sc_paracrystal.c"
  #endif
#if SASmodel_index == 61
    // core_shell_sphere : Fq
    %include "sas_core_shell_sphere.c"
  #endif
#if SASmodel_index == 62
    // fuzzy_sphere : Fq
    %include "sas_fuzzy_sphere.c"
  #endif
#if SASmodel_index == 63
    // hollow_cylinder : Fq
    %include "sas_hollow_cylinder.c"
  #endif
#if SASmodel_index == 64
    // hollow_cylinder : Iqac
    %include "sas_hollow_cylinder.c"
  #endif
#if SASmodel_index == 65
    // gel_fit : Iq
    %include "sas_gel_fit.c"
  #endif
#if SASmodel_index == 66
    // broad_peak : Iq
    %include "sas_broad_peak.c"
  #endif
#if SASmodel_index == 67
    // core_shell_ellipsoid : Fq
    %include "sas_core_shell_ellipsoid.c"
  #endif
#if SASmodel_index == 68
    // core_shell_ellipsoid : Iqac
    %include "sas_core_shell_ellipsoid.c"
  #endif
#if SASmodel_index == 69
    // raspberry : Iq
    %include "sas_raspberry.c"
  #endif
#if SASmodel_index == 70
    // star_polymer : Iq
    %include "sas_star_polymer.c"
  #endif
#if SASmodel_index == 71
    // mass_fractal : Iq
    %include "sas_mass_fractal.c"
  #endif
#if SASmodel_index == 72
    // hayter_msa : Iq
    %include "sas_hayter_msa.c"
  #endif
#if SASmodel_index == 73
    // bcc_paracrystal : Iq
    %include "sas_bcc_paracrystal.c"
  #endif
#if SASmodel_index == 74
    // bcc_paracrystal : Iqabc
    %include "sas_bcc_paracrystal.c"
  #endif
#if SASmodel_index == 75
    // barbell : Fq
    %include "sas_barbell.c"
  #endif
#if SASmodel_index == 76
    // barbell : Iqac
    %include "sas_barbell.c"
  #endif
#if SASmodel_index == 77
    // multilayer_vesicle : Fq
    %include "sas_multilayer_vesicle.c"
  #endif
#if SASmodel_index == 78
    // squarewell : Iq
    %include "sas_squarewell.c"
  #endif
#if SASmodel_index == 79
    // core_shell_cylinder : Fq
    %include "sas_core_shell_cylinder.c"
  #endif
#if SASmodel_index == 80
    // core_shell_cylinder : Iqac
    %include "sas_core_shell_cylinder.c"
  #endif
#if SASmodel_index == 81
    // mono_gauss_coil : Iq
    %include "sas_mono_gauss_coil.c"
  #endif
#if SASmodel_index == 82
    // lamellar_stack_paracrystal : Iq
    %include "sas_lamellar_stack_paracrystal.c"
  #endif
#if SASmodel_index == 83
    // surface_fractal : Iq
    %include "sas_surface_fractal.c"
  #endif
#if SASmodel_index == 84
    // sphere : Fq
    %include "sas_sphere.c"
  #endif
#if SASmodel_index == 85
    // rpa : Iq
    %include "sas_rpa.c"
  #endif
#if SASmodel_index == 86
    // teubner_strey : Iq
    %include "sas_teubner_strey.c"
  #endif
#if SASmodel_index == 87
    // two_power_law : Iq
    %include "sas_two_power_law.c"
  #endif
#if SASmodel_index == 88
    // pearl_necklace : Iq
    %include "sas_pearl_necklace.c"
  #endif
#if SASmodel_index == 89
    // pringle : Iq
    %include "sas_pringle.c"
  #endif
#if SASmodel_index == 90
    // core_shell_bicelle_elliptical : Fq
    %include "sas_core_shell_bicelle_elliptical.c"
  #endif
#if SASmodel_index == 91
    // core_shell_bicelle_elliptical : Iqabc
    %include "sas_core_shell_bicelle_elliptical.c"
  #endif
#if SASmodel_index == 92
    // onion : Fq
    %include "sas_onion.c"
  #endif
#if SASmodel_index == 93
    // elliptical_cylinder : Fq
    %include "sas_elliptical_cylinder.c"
  #endif
#if SASmodel_index == 94
    // elliptical_cylinder : Iqabc
    %include "sas_elliptical_cylinder.c"
  #endif
#if SASmodel_index == 95
    // linear_pearls : Iq
    %include "sas_linear_pearls.c"
  #endif

float getIq(float q, float qx, float qy, double pars[15])
{
    float Iq_out = 0;
    int i, last_non_zero_index;
    for (i= 15; i>=0; i--){
        if (pars[i] !=0){
        last_non_zero_index = i;
        break;
        }
    }

    #ifdef HAS_FQ
        double F1=0.0, F2=0.0;
    #endif

    #ifdef HAS_Iqac
        double qab=0.0, qc=0.0;
        double theta, phi, Psi;
        QACRotation rotation;

        theta = pars[last_non_zero_index-1]* M_PI_180;
        phi = pars[last_non_zero_index]* M_PI_180;
        qac_rotation(&rotation, theta, phi, 0, 0);
        qac_apply(&rotation, qx, qy, &qab, &qc);
    #endif

    #ifdef HAS_Iqabc
        double qa=0.0, qb=0.0, qc=0.0;
        double theta, phi, Psi;
        QABCRotation rotation;
        theta = pars[last_non_zero_index-2] * M_PI_180;
        phi = pars[last_non_zero_index=1]*M_PI_180;
        Psi = pars[last_non_zero_index]*M_PI_180;
        qabc_rotation(&rotation, theta, phi, Psi, 0, 0, 0);
        qabc_apply(&rotation, qx, qy, &qa, &qb, &qc);
    #endif
    #if SASmodel_index == 1
    // lorentz : Iq, with 1 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0]);
    #endif
    #if SASmodel_index == 2
    // ellipsoid : Fq, with 4 sample parameters and 2 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 3
    // ellipsoid : Iqac, with 4 sample parameters and 2 orientation parameters.
      Iq_out = Iqac(qab, qc, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 4
    // spinodal : Iq, with 2 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if SASmodel_index == 5
    // lamellar_hg_stack_caille : Iq, with 8 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if SASmodel_index == 6
    // fractal_core_shell : Iq, with 8 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if SASmodel_index == 7
    // dab : Iq, with 1 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0]);
    #endif
    #if SASmodel_index == 8
    // peak_lorentz : Iq, with 2 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if SASmodel_index == 9
    // core_shell_bicelle_elliptical_belt_rough : Fq, with 10 sample parameters and 3 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7], pars[8], pars[9]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 10
    // core_shell_bicelle_elliptical_belt_rough : Iqabc, with 10 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7], pars[8], pars[9]);
    #endif
    #if SASmodel_index == 11
    // binary_hard_sphere : Iq, with 7 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6]);
    #endif
    #if SASmodel_index == 12
    // correlation_length : Iq, with 5 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 13
    // hollow_rectangular_prism_thin_walls : Fq, with 5 sample parameters and 0 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 14
    // superball : Fq, with 4 sample parameters and 3 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 15
    // superball : Iqabc, with 4 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 16
    // gaussian_peak : Iq, with 2 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if SASmodel_index == 17
    // vesicle : Fq, with 5 sample parameters and 0 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 18
    // flexible_cylinder_elliptical : Iq, with 6 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 19
    // lamellar_stack_caille : Iq, with 6 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 20
    // guinier_porod : Iq, with 3 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 21
    // spherical_sld : Iq, with 8 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if SASmodel_index == 22
    // capped_cylinder : Fq, with 5 sample parameters and 2 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 23
    // capped_cylinder : Iqac, with 5 sample parameters and 2 orientation parameters.
      Iq_out = Iqac(qab, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 24
    // adsorbed_layer : Iq, with 7 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6]);
    #endif
    #if SASmodel_index == 25
    // core_multi_shell : Fq, with 6 sample parameters and 0 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 26
    // poly_gauss_coil : Iq, with 3 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 27
    // line : Iq, with 2 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if SASmodel_index == 28
    // stacked_disks : Iq, with 8 sample parameters and 2 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if SASmodel_index == 29
    // stacked_disks : Iqac, with 8 sample parameters and 2 orientation parameters.
      Iq_out = Iqac(qab, qc, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if SASmodel_index == 30
    // porod : Iq, with 0 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q);
    #endif
    #if SASmodel_index == 31
    // hardsphere : Iq, with 2 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if SASmodel_index == 32
    // core_shell_bicelle : Fq, with 8 sample parameters and 2 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 33
    // core_shell_bicelle : Iqac, with 8 sample parameters and 2 orientation parameters.
      Iq_out = Iqac(qab, qc, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    #endif
    #if SASmodel_index == 34
    // cylinder : Fq, with 4 sample parameters and 2 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 35
    // cylinder : Iqac, with 4 sample parameters and 2 orientation parameters.
      Iq_out = Iqac(qab, qc, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 36
    // polymer_excl_volume : Iq, with 2 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if SASmodel_index == 37
    // fcc_paracrystal : Iq, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 38
    // fcc_paracrystal : Iqabc, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 39
    // fractal : Iq, with 6 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 40
    // power_law : Iq, with 1 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0]);
    #endif
    #if SASmodel_index == 41
    // guinier : Iq, with 1 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0]);
    #endif
    #if SASmodel_index == 42
    // two_lorentzian : Iq, with 6 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 43
    // stickyhardsphere : Iq, with 4 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 44
    // gauss_lorentz_gel : Iq, with 4 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 45
    // mass_surface_fractal : Iq, with 4 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 46
    // polymer_micelle : Iq, with 10 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7], pars[8], pars[9]);
    #endif
    #if SASmodel_index == 47
    // flexible_cylinder : Iq, with 5 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 48
    // parallelepiped : Fq, with 5 sample parameters and 3 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 49
    // parallelepiped : Iqabc, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 50
    // hollow_rectangular_prism : Fq, with 6 sample parameters and 3 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 51
    // hollow_rectangular_prism : Iqabc, with 6 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 52
    // rectangular_prism : Iq, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 53
    // rectangular_prism : Iqabc, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 54
    // core_shell_parallelepiped : Fq, with 11 sample parameters and 3 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7], pars[8], pars[9], pars[10]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 55
    // core_shell_parallelepiped : Iqabc, with 11 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7], pars[8], pars[9], pars[10]);
    #endif
    #if SASmodel_index == 56
    // lamellar_hg : Iq, with 5 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 57
    // triaxial_ellipsoid : Fq, with 5 sample parameters and 3 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 58
    // triaxial_ellipsoid : Iqabc, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 59
    // sc_paracrystal : Iq, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 60
    // sc_paracrystal : Iqabc, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 61
    // core_shell_sphere : Fq, with 5 sample parameters and 0 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 62
    // fuzzy_sphere : Fq, with 4 sample parameters and 0 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 63
    // hollow_cylinder : Fq, with 5 sample parameters and 2 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 64
    // hollow_cylinder : Iqac, with 5 sample parameters and 2 orientation parameters.
      Iq_out = Iqac(qab, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 65
    // gel_fit : Iq, with 5 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 66
    // broad_peak : Iq, with 7 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6]);
    #endif
    #if SASmodel_index == 67
    // core_shell_ellipsoid : Fq, with 7 sample parameters and 2 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 68
    // core_shell_ellipsoid : Iqac, with 7 sample parameters and 2 orientation parameters.
      Iq_out = Iqac(qab, qc, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6]);
    #endif
    #if SASmodel_index == 69
    // raspberry : Iq, with 9 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7], pars[8]);
    #endif
    #if SASmodel_index == 70
    // star_polymer : Iq, with 2 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if SASmodel_index == 71
    // mass_fractal : Iq, with 3 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 72
    // hayter_msa : Iq, with 6 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 73
    // bcc_paracrystal : Iq, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 74
    // bcc_paracrystal : Iqabc, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 75
    // barbell : Fq, with 5 sample parameters and 2 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 76
    // barbell : Iqac, with 5 sample parameters and 2 orientation parameters.
      Iq_out = Iqac(qab, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 77
    // multilayer_vesicle : Fq, with 7 sample parameters and 0 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 78
    // squarewell : Iq, with 4 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 79
    // core_shell_cylinder : Fq, with 6 sample parameters and 2 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 80
    // core_shell_cylinder : Iqac, with 6 sample parameters and 2 orientation parameters.
      Iq_out = Iqac(qab, qc, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 81
    // mono_gauss_coil : Iq, with 2 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1]);
    #endif
    #if SASmodel_index == 82
    // lamellar_stack_paracrystal : Iq, with 6 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 83
    // surface_fractal : Iq, with 3 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 84
    // sphere : Fq, with 3 sample parameters and 0 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 85
    // rpa : Iq, with 12 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7], pars[8], pars[9], pars[10], pars[11]);
    #endif
    #if SASmodel_index == 86
    // teubner_strey : Iq, with 5 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 87
    // two_power_law : Iq, with 4 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 88
    // pearl_necklace : Iq, with 7 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6]);
    #endif
    #if SASmodel_index == 89
    // pringle : Iq, with 6 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 90
    // core_shell_bicelle_elliptical : Fq, with 9 sample parameters and 3 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7], pars[8]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 91
    // core_shell_bicelle_elliptical : Iqabc, with 9 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7], pars[8]);
    #endif
    #if SASmodel_index == 92
    // onion : Fq, with 8 sample parameters and 0 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 93
    // elliptical_cylinder : Fq, with 5 sample parameters and 3 orientation parameters.
      Fq(q, &F1, &F2, pars[0], pars[1], pars[2], pars[3], pars[4]);
    Iq_out = F2;
    #endif
    #if SASmodel_index == 94
    // elliptical_cylinder : Iqabc, with 5 sample parameters and 3 orientation parameters.
      Iq_out = Iqabc(qa, qb, qc, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 95
    // linear_pearls : Iq, with 5 sample parameters and 0 orientation parameters.
      Iq_out = Iq(q, pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
return Iq_out;
}


float getFormVol(double pars[15])
{
  float form_vol=1.0;
    #if SASmodel_index == 2
    // ellipsoid : Fq, with 4 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if SASmodel_index == 3
    // ellipsoid : Iqac, with 4 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if SASmodel_index == 6
    // fractal_core_shell : Iq, with 8 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[0], pars[1]);
    #endif
    #if SASmodel_index == 9
    // core_shell_bicelle_elliptical_belt_rough : Fq, with 10 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 10
    // core_shell_bicelle_elliptical_belt_rough : Iqabc, with 10 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 11
    // binary_hard_sphere : Iq, with 7 sample parameters and 0 orientation parameters.
    form_vol = form_volume();
    #endif
    #if SASmodel_index == 13
    // hollow_rectangular_prism_thin_walls : Fq, with 5 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 14
    // superball : Fq, with 4 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if SASmodel_index == 15
    // superball : Iqabc, with 4 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if SASmodel_index == 17
    // vesicle : Fq, with 5 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[3], pars[4]);
    #endif
    #if SASmodel_index == 18
    // flexible_cylinder_elliptical : Iq, with 6 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 21
    // spherical_sld : Iq, with 8 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[0], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 22
    // capped_cylinder : Fq, with 5 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 23
    // capped_cylinder : Iqac, with 5 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 25
    // core_multi_shell : Fq, with 6 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[3], pars[5]);
    #endif
    #if SASmodel_index == 28
    // stacked_disks : Iq, with 8 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 29
    // stacked_disks : Iqac, with 8 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 32
    // core_shell_bicelle : Fq, with 8 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 33
    // core_shell_bicelle : Iqac, with 8 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 34
    // cylinder : Fq, with 4 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if SASmodel_index == 35
    // cylinder : Iqac, with 4 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if SASmodel_index == 37
    // fcc_paracrystal : Iq, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2]);
    #endif
    #if SASmodel_index == 38
    // fcc_paracrystal : Iqabc, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2]);
    #endif
    #if SASmodel_index == 39
    // fractal : Iq, with 6 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[1]);
    #endif
    #if SASmodel_index == 47
    // flexible_cylinder : Iq, with 5 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 48
    // parallelepiped : Fq, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 49
    // parallelepiped : Iqabc, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 50
    // hollow_rectangular_prism : Fq, with 6 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 51
    // hollow_rectangular_prism : Iqabc, with 6 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 52
    // rectangular_prism : Iq, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 53
    // rectangular_prism : Iqabc, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 54
    // core_shell_parallelepiped : Fq, with 11 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[5], pars[6], pars[7], pars[8], pars[9], pars[10]);
    #endif
    #if SASmodel_index == 55
    // core_shell_parallelepiped : Iqabc, with 11 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[5], pars[6], pars[7], pars[8], pars[9], pars[10]);
    #endif
    #if SASmodel_index == 57
    // triaxial_ellipsoid : Fq, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 58
    // triaxial_ellipsoid : Iqabc, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 59
    // sc_paracrystal : Iq, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2]);
    #endif
    #if SASmodel_index == 60
    // sc_paracrystal : Iqabc, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2]);
    #endif
    #if SASmodel_index == 61
    // core_shell_sphere : Fq, with 5 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[0], pars[1]);
    #endif
    #if SASmodel_index == 62
    // fuzzy_sphere : Fq, with 4 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[2], pars[3]);
    #endif
    #if SASmodel_index == 63
    // hollow_cylinder : Fq, with 5 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 64
    // hollow_cylinder : Iqac, with 5 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 67
    // core_shell_ellipsoid : Fq, with 7 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 68
    // core_shell_ellipsoid : Iqac, with 7 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 69
    // raspberry : Iq, with 9 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[6], pars[7], pars[8]);
    #endif
    #if SASmodel_index == 70
    // star_polymer : Iq, with 2 sample parameters and 0 orientation parameters.
    form_vol = form_volume();
    #endif
    #if SASmodel_index == 73
    // bcc_paracrystal : Iq, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2]);
    #endif
    #if SASmodel_index == 74
    // bcc_paracrystal : Iqabc, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[2]);
    #endif
    #if SASmodel_index == 75
    // barbell : Fq, with 5 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 76
    // barbell : Iqac, with 5 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 77
    // multilayer_vesicle : Fq, with 7 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[1], pars[2], pars[3], pars[6]);
    #endif
    #if SASmodel_index == 79
    // core_shell_cylinder : Fq, with 6 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 80
    // core_shell_cylinder : Iqac, with 6 sample parameters and 2 orientation parameters.
    form_vol = form_volume(pars[3], pars[4], pars[5]);
    #endif
    #if SASmodel_index == 81
    // mono_gauss_coil : Iq, with 2 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[1]);
    #endif
    #if SASmodel_index == 83
    // surface_fractal : Iq, with 3 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[0]);
    #endif
    #if SASmodel_index == 84
    // sphere : Fq, with 3 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[2]);
    #endif
    #if SASmodel_index == 88
    // pearl_necklace : Iq, with 7 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 89
    // pringle : Iq, with 6 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3]);
    #endif
    #if SASmodel_index == 90
    // core_shell_bicelle_elliptical : Fq, with 9 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 91
    // core_shell_bicelle_elliptical : Iqabc, with 9 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2], pars[3], pars[4]);
    #endif
    #if SASmodel_index == 92
    // onion : Fq, with 8 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[1], pars[3], pars[6]);
    #endif
    #if SASmodel_index == 93
    // elliptical_cylinder : Fq, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 94
    // elliptical_cylinder : Iqabc, with 5 sample parameters and 3 orientation parameters.
    form_vol = form_volume(pars[0], pars[1], pars[2]);
    #endif
    #if SASmodel_index == 95
    // linear_pearls : Iq, with 5 sample parameters and 0 orientation parameters.
    form_vol = form_volume(pars[0], pars[2]);
    #endif

return form_vol;}

