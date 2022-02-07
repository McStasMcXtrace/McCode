%define name			mcstas-suite-python
%define summary			A metapackage for McStas + python tools
%define version			2.2
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
Requires:  mcstas-2.2 mcstas-comps-2.2 mcstas-tools-python-mcplot-matplotlib-2.2 mcstas-tools-python-mcdisplay-matplotlib-2.2 mcstas-tools-python-mcrun-2.2 mcstas-tools-python-mcgui-2.2 mcstas-manuals-2.2

%description
A metapackage for McStas + python tools

%files
