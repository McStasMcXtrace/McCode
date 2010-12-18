<?xml version="1.0"?>
<!--
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!                                   Jiao Lin
!                      California Institute of Technology
!                      (C) 2006-2010  All Rights Reserved
!
! {LicenseText}
!
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-->

<!DOCTYPE inventory>

<inventory>

    <component name="IQE_monitor_TestCase-test2">
        <property name="sequence">['source', 'sample', 'detector']</property>
        <facility name="source">sources/MonochromaticSource</facility>
        <facility name="sample">samples/SampleAssemblyFromXml</facility>
        <facility name="detector">monitors/IQE_monitor</facility>

        <property name="multiple-scattering">False</property>

        <property name="ncount">10000.0</property>
        <property name="buffer_size">1000</property>

        <property name="output-dir">out</property>
        <property name="overwrite-datafiles">False</property>

        <component name="source">
            <property name="position">[0.0, 0.0, 0.0]</property>
            <property name="energy">60.0</property>
            <property name="velocity">[0.0, 0.0, 1.0]</property>
            <property name="probability">1.0</property>
            <property name="time">0.0</property>
        </component>


        <component name="sample">
            <property name="xml">isotropic-sqe/sampleassembly.xml</property>
        </component>


        <component name="detector">
            <property name="name">iqe_monitor</property>
            <property name="Ei">60.0</property>
            <property name="Emax">55.0</property>
            <property name="Emin">-55.0</property>
            <property name="nE">220</property>
            <property name="Qmin">0.0</property>
            <property name="Qmax">10.0</property>
            <property name="nQ">200</property>
            <property name="min_angle_in_plane">0.0</property>
            <property name="max_angle_in_plane">120.0</property>
            <property name="min_angle_out_of_plane">-30.0</property>
            <property name="max_angle_out_of_plane">30.0</property>
            <property name="filename">iqe_monitor.dat</property>
        </component>


        <component name="geometer">
            <property name="source">((0, 0, 0), (0, 0, 0))</property>
            <property name="sample">((0, 0, 3), (0, 0, 0))</property>
            <property name="detector">((0, 0, 3), (0, 0, 0))</property>
        </component>

    </component>

</inventory>

<!-- version-->
<!-- $Id$-->

<!-- End of file -->


