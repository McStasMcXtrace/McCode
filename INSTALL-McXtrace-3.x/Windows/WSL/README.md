# Installation of McXtrace 3.2 on Windows 64 bit systems - using WSL
*(WSL is the Windows Subsystem for Linux, aka. bash on Ubuntu on Windows)*


## Enable bash
(From https://stackoverflow.com/questions/36352627/how-to-enable-bash-in-windows-10-developer-preview)
* Click the Start button,
* Write Control Panel and start the Control Panel app
* Click Programs
* Click Turn Windows features on or off
* Enable Windows Subsystem for Linux
* Let the machine reboot if necessary
* Open the Windows store
* Search for Ubuntu
* Install Ubuntu 16.04 LTS or 18.04 LTS (other Linuxes are also
available, but we recommend Ubuntu)
* Click Launch to run the app and follow on-screen instructions
* To open it again later, simply issue bash in a terminal or through
the start menu

## Install the McXtrace 3.2 Debian packages
* Follow the
  [normal Debian installation instructions](../../Linux/debian/README.md)
  - essentially a matter of sudo apt-get install mcxtrace-suite-python mcxtrace-suite-perl

##  Install Xming or another X11 server application
* Download and install Xming via https://sourceforge.net/projects/xming/
* Windows store also provides a $-ware called X410 from "Choung Networks"

## Optionally add a few commands at the end of your .bashrc:
```bash
cd $HOME
nano .bashrc
```
At the end, add the lines below, substituting pwill for  your windows
user name
```bash
# cd to the home dir of your windows user
cd /mnt/c/Users/pwill
# Add the DISPLAY variable to talk to Xming
export DISPLAY=:0.0
```

## Start mcgui
* Ensure your X11 server is running
* Start bash and ensure the DISPLAY variable is set (export
DISPLAY=:0.0)
* Issue the command mcgui

## Configuring McXtrace to use the Windows browser
* Ensure the package ubuntu-wsl (or equivalent) is installed
* Set the BROWSER="wslview" in your /usr/share/mcxtrace/3.2/tools/Python/mccodelib/mccode_config.py

## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)

