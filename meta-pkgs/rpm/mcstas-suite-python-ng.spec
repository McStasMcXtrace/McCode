%define name			mcstas-suite-python-ng
%define summary			A metapackage for McStas + python tools
%define version			@VERSION@
%define release			1
%define license			GPL
%define group			Documentation
%define vendor			mccode.org
%define packager		Peter willendrup

Name:      %{name}
Version:   %{version}
Release:   %{release}
Packager:  %{packager}
Vendor:    %{vendor}
License:   %{license}
Summary:   %{summary}
Group:     %{group}
URL:       http://www.mccode.org
Buildroot: %{buildroot}
Requires:  mcstas-@VERSION@ mcstas-comps-@VERSION@ mcstas-tools-python-mcplot-pyqtgraph-@VERSION@ mcstas-tools-python-mcplot-matplotlib-@VERSION@ mcstas-tools-python-mcrun-@VERSION@ mcstas-tools-python-mcgui-@VERSION@ mcstas-tools-python-mccodelib-@VERSION@ mcstas-tools-python-mcdisplay-webgl-@VERSION@ mcstas-tools-python-mcdisplay-pyqtgraph-@VERSION@ mcstas-tools-python-mcdisplay-mantid-@VERSION@ mcstas-tools-python-mcdoc-@VERSION@ mcstas-manuals-@VERSION@ mcstas-mcpl-@VERSION@

%description
A metapackage for McStas + python tools

%files
