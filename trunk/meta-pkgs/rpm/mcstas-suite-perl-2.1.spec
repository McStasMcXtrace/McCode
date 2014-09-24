%define name			mcstas-suite-perl-2.1
%define summary			A metapackage for McStas + perl tools
%define version			2.1
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
Requires:  mcstas-2.1 mcstas-comps-2.1 mcstas-tools-perl-2.1 mcstas-manuals-2.1

%description
A metapackage for McStas + perl tools

%files
