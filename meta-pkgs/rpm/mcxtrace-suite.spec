%define name			mcxtrace-suite
%define summary			A metapackage for McXtrace + perl and python tools
%define version			1.1
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
Requires:  mcxtrace-suite-perl mcxtrace-suite-python

%description
A metapackage for McXtrace + perl and python tools

%files
