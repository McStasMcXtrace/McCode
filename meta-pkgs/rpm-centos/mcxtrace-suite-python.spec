%define name			mcxtrace-suite-python-ng
%define summary			A metapackage for McXtrace + python tools
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
Requires:  mcxtrace-@VERSION@ mcxtrace-comps-@VERSION@ mcxtrace-tools-perl-cmdline-@VERSION@ mcxtrace-tools-python-mxplot-pyqtgraph-@VERSION@ mcxtrace-tools-python-mxplot-matplotlib-@VERSION@ mcxtrace-tools-python-mxrun-@VERSION@ mcxtrace-tools-python-mxgui-@VERSION@ mcxtrace-tools-python-mccodelib-@VERSION@ mcxtrace-tools-python-mxdisplay-webgl-@VERSION@ mcxtrace-tools-python-mxdisplay-pyqtgraph-@VERSION@ mcxtrace-tools-python-mcdoc-@VERSION@ mcxtrace-manuals-@VERSION@ mcxtrace-miniconda3-@VERSION@

%description
A metapackage for McXtrace + python tools

%files
