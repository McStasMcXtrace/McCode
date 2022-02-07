%define name			mcstas-suite-perl
%define summary			A metapackage for McStas + perl tools
%define version			2.2a
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
Requires:  mcstas-2.2a mcstas-comps-2.2a mcstas-tools-perl-2.2a mcstas-manuals-2.2a

%description
A metapackage for McStas + perl tools

%files
