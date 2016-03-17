%define name			mcstas-suite
%define summary			A metapackage for McStas + perl and python tools
%define version			2.0
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
Requires:  mcstas-suite-perl-2.0 mcstas-suite-python-2.0

%description
A metapackage for McStas + perl and python tools

%files
