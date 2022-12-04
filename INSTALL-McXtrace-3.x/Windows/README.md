# Installation of McXtrace 3.0 on Windows 64 bit systems


## IMPORTANT A:
* If you are behind a proxy server, please use the commands
	```bash
		set HTTP_PROXY=http://your_proxy_ip:port
	```
	```bash
		set HTTPS_PROXY=https://your_proxy_ip:port
	```
in a cmd.exe shell and start the Perl and McXtrace installers from there
	
##  IMPORTANT B:
* Please install [strawberry-perl-5.26.3.1-64bit.msi](http://download.mcxtrace.org/mcxtrace-3.0/windows/mcxtrace-win64/strawberry-perl-5.26.3.1-64bit.msi)
BEFORE  attempting to install the McXtrace metapackage!!!

## IMPORTANT C:
* Please install the [McXtrace 3.0 metapackage](http://download.mcxtrace.org/mcxtrace-3.0/windows/McXtrace-Metapackage-3.0-win64.exe)

* The [extras](http://download.mcxtrace.org/mcxtrace-3.0/windows/extras)
  folder provides binaries for Microsoft MPI and NeXus, which can be utilised by the McXtrace metapackage installation



* An alternative to installing this version is to follow the instructions
posted under [WSL](WSL/README.md) to install the 
"Windows subsystem for Linux" and run the Debian binaries there

## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)

