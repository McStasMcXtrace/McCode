%define name			mcxtrace-suite-perl
%define summary			A metapackage for McXtrace + perl tools
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
Requires:  mcxtrace-1.1 mcxtrace-comps-1.1 mcxtrace-tools-perl-1.1

%description
A metapackage for McXtrace + perl tools

%files
