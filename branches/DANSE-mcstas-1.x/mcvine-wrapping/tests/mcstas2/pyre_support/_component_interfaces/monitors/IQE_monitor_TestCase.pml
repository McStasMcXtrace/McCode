<?xml version="1.0"?>
<!--
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!                                   Jiao Lin
!                      California Institute of Technology
!                        (C) 2007  All Rights Reserved
!
! {LicenseText}
!
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-->

<!DOCTYPE inventory>

<inventory>

    <component name="testiqem">
        <property name="output-dir">out</property>
        <property name="sequence">['source', 'monitor']</property>
        <property name="ncount">10</property>
        <property name="overwrite-datafiles">False</property>
        <property name="buffer_size">10</property>

        <component name="source">
            <property name="yh">0.1</property>
            <property name="dist">10.0</property>
            <property name="width">0.0</property>
            <property name="dE">10.0</property>
            <property name="gauss">0.0</property>
            <property name="height">0.0</property>
            <property name="flux">1.0</property>
            <property name="dLambda">0.0</property>
            <property name="radius">0.02</property>
            <property name="Lambda0">0.0</property>
            <property name="E0">60.0</property>
            <property name="xw">0.1</property>
        </component>


        <component name="monitor">
            <property name="Ei">60.0</property>
            <property name="Emin">-50.0</property>
            <property name="Emax"> 50.0</property>
            <property name="nE">100</property>
            <property name="Qmin">0</property>
            <property name="Qmax">13</property>
            <property name="nQ">130</property>
            <property name="max_angle_out_of_plane">90</property>
            <property name="min_angle_out_of_plane">-90</property>
            <property name="max_angle_in_plane">180</property>
            <property name="min_angle_in_plane">-180</property>
        </component>


        <component name="geometer">
            <property name="monitor">(0,0,10),(0,0,0)</property>
            <property name="source">(0,0,0),(0,0,0)</property>
        </component>

    </component>

</inventory>

<!-- version-->
<!-- $Id: IQE_monitor_TestCase.pml 599 2010-10-03 18:18:31Z linjiao $-->

<!-- Generated automatically by Renderer on Mon Feb  4 17:25:06 2008-->

<!-- End of file -->
