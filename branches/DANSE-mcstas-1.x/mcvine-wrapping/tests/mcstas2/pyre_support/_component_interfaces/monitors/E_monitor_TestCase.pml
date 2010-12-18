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

    <component name="E_monitor_TestCase">

        <property name="output-dir">E_monitor_TestCase-out</property>
        <property name="sequence">['source', 'monitor']</property>
        <property name="ncount">100</property>
        <property name="buffer_size">10</property>
        <property name="overwrite-datafiles">1</property>

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
            <property name="Emin">10.0</property>
            <property name="Emax">100.0</property>
            <property name="filename">IE.dat</property>
            <property name="nchan">20</property>
            <property name="xmin">-5.0</property>
            <property name="xmax">5.0</property>
            <property name="ymin">-5.0</property>
            <property name="ymax">5.0</property>
        </component>


        <component name="geometer">
            <property name="monitor">(0,0,10),(0,0,0)</property>
            <property name="source">(0,0,0),(0,0,0)</property>
        </component>

    </component>

</inventory>

<!-- version-->
<!-- $Id: E_monitor_TestCase.pml 672 2010-11-01 14:52:39Z linjiao $-->

<!-- Generated automatically by Renderer on Mon Feb  4 17:25:06 2008-->

<!-- End of file -->
