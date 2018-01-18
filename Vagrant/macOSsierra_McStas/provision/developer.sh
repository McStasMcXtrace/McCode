# For a full update, one would run
# softwareupdate -iva

# Get Xcode commandline tools
touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress;
PROD=$(softwareupdate -l |
 grep "\*.*Command Line" |
 head -n 1 | awk -F"*" '{print $2}' |
 sed -e 's/^ *//' |
 tr -d '\n')
softwareupdate -i "$PROD" --verbose;

WORKDIR=`pwd`

# Get gfortran from hpc.sourceforge.net
cd /tmp
curl -O https://10gbps-io.dl.sourceforge.net/project/hpc/hpc/g95/gfortran-7.1-bin.tar.gz
cd /
tar xzf /tmp/gfortran-7.1-bin.tar.gz

cd $WORKDIR

# Get and install CMake
curl -O https://cmake.org/files/v3.10/cmake-3.10.1-Darwin-x86_64.tar.gz
tar xzf cmake-3.10.1-Darwin-x86_64.tar.gz
cp -rp cmake-3.10.1-Darwin-x86_64/CMake.app /Applications
sudo mkdir -p /usr/local/bin
sudo ln -sf /Applications/CMake.app/Contents/bin/cmake /usr/local/bin
sudo ln -sf /Applications/CMake.app/Contents/bin/cpack /usr/local/bin

sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1
cd McCode
echo "#!/bin/bash" > go.command
echo cd /Users/vagrant/McCode >> go.command
echo ./build_macos_mcstas 2.5beta01 >> go.command
chown vagrant:staff go.command
chmod a+x go.command
sudo -u vagrant open go.command
