#!/bin/sh

sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1
cd McCode
sudo -u vagrant git pull
echo "#!/bin/bash" > go.command
echo cd /Users/vagrant/McCode >> go.command
echo ./build_macos_mcstas 2.5beta01 >> go.command
chown vagrant:staff go.command
chmod a+x go.command
sudo -u vagrant open go.command
