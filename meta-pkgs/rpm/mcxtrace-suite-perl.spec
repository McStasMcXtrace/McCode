%define name			mcxtrace-suite-perl-ng
%define summary			A metapackage for McXtrace + perl tools
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
Requires:  mcxtrace-@VERSION@ mcxtrace-comps-@VERSION@ mcxtrace-tools-perl-@VERSION@

%description
A metapackage for McXtrace + perl tools

%files
