## Install McStas 3.5.12 On Debian class systems (including Ubuntu, mint etc.):
The packages have been tested to work correctly on Ubuntu 22.04 and Debian 11.

# For McStas 3.5.12 only, installation includes the following manual steps:

1. Please **uninstall** McStas 3.5.1 if it exists on your system
2. Please manually download the 3.5.12 packages from https://download.mcstas.org/mcstas-3.5.12/Linux/debian/
3. Unpack and install the packages using e.g. dpkg

# Using mcdoc on modern Ubuntu systems
Ubuntu is shipping its browsers as "snap" packages, meaning that they
are blocked from accessing e.g. the McStas html snippets in
/usr/share/mcstas/3.5.12/.

To fix this we propose to switch your browser to a proper apt based
installation, in this example firefox:

Remove the snap-based firefox:
```
sudo snap remove firefox
```
Add the official mozilla-built firefox instead:
```
sudo add-apt-repository ppa:mozillateam/ppa
```
Set priorities to always prefer this firefox package:
```
echo '
Package: *
Pin: release o=LP-PPA-mozillateam
Pin-Priority: 1001
' | sudo tee /etc/apt/preferences.d/mozilla-firefox
```
Set up unattended upgrades of firefox
```
echo 'Unattended-Upgrade::Allowed-Origins::
"LP-PPA-mozillateam:${distro_codename}";' | sudo tee
/etc/apt/apt.conf.d/51unattended-upgrades-firefox
```
Finally, install firefox from the mozilla-channels
```
sudo apt install firefox
```

## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)


