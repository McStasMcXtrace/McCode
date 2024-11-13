# Installation of McXtrace 3.5.1 on Windows 64 bit systems

* Please install the [McXtrace 3.5.1 metapackage](https://download.mcxtrace.org/mcxtrace-3.5.1/Windows/McXtrace-Metapackage-3.5.1-win64.exe)
* To enable use of MCPL with McXtrace 3.5.1 on Windows, please:
 1) Locate and run the executable `mcxtrace-mcpl-NSIS64-3.5.1-mingw64.exe` from the [single-packages folder](https://download.mcxtrace.org/mcxtrace-3.5.1/Windows/single-packages)
 2) During installation, please specify `c:\mcxtrace-3.5.1` as installation directory
 3) After installation, place the mcpl-related `.bat` files from the [extras folder](https://download.mcxtrace.org/mcxtrace-3.5.1/Windows/extras) folder in `c:\mcxtrace-3.5.1\bin`


* An alternative to installing this cross-compiled verison is to follow the instructions
posted under [WSL](WSL/README.md) to install the "Windows subsystem for Linux" and run the Debian binaries there
* Or use [conda-forge](../conda/README.md)

## Internet access required:
* If you are behind a proxy server, please use the commands
	```bash
		set HTTP_PROXY=http://your_proxy_ip:port
	```
	```bash
		set HTTPS_PROXY=https://your_proxy_ip:port
	```
in a cmd.exe shell and start the McXtrace installer from there	

## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)

