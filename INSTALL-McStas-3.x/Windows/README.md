# Installation of McStas 3.2 on Windows 64 bit systems


## IMPORTANT A:
* If you are behind a proxy server, please use the commands
	```bash
		set HTTP_PROXY=http://your_proxy_ip:port
	```
	```bash
		set HTTPS_PROXY=https://your_proxy_ip:port
	```
in a cmd.exe shell and start the Perl and McStas installers from there
	
##  IMPORTANT B:
* Please install [strawberry-perl-5.26.3.2-64bit.msi](https://download.mcstas.org/mcstas-3.2/windows/strawberry-perl-5.26.3.2-64bit.msi)
BEFORE  attempting to install the McStas metapackage!!!

## IMPORTANT C:
* Please install the [McStas 3.2 metapackage](https://download.mcstas.org/mcstas-3.2/windows/McStas-Metapackage-3.2-win64.exe)

* The [extras](https://download.mcstas.org/mcstas-3.2/windows/extras)
  folder provides binaries for Microsoft MPI and NeXus, which can be utilised by the McStas metapackage installation



* An alternative to installing this version is to follow the instructions
posted under [WSL](WSL/README.md) to install the 
"Windows subsystem for Linux" and run the Debian binaries there

## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

