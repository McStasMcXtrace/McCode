%define name			mcstas-suite-perl-ng
%define summary			A metapackage for McStas + perl tools
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
Requires:  mcstas-@VERSION@ mcstas-comps-@VERSION@ mcstas-tools-perl-@VERSION@ mcstas-tools-perl-cmdline-@VERSION@ mcstas-manuals-@VERSION@

%description
A metapackage for McStas + perl tools

%files
