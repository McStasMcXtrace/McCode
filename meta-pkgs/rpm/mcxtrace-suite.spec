%define name			mcxtrace-suite
%define summary			A metapackage for McXtrace + perl and python tools
%define version			@VERSION@
%define release			1
%define license			GPL
%define group			Documentation
%define vendor			mccode.org
%define packager		Erik B Knudsen

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
