#! /bin/bash

# Install basic developer tools 

sudo yum update
sudo yum -y upgrade
sudo yum install -y git flex bison rpm-build gcc-gfortran 
sudo yum install -y epel-release
sudo yum update
sudo yum -y upgrade
sudo yum install -y cmake3 perl-Extuils-Makemaker
sudo ln -s /bin/cmake3 /usr/bin/cmake
sudo ln -s /bin/cpack3 /usr/bin/cpack
curl -O http://packages.mccode.org/rpm/mccode.repo
sudo cp mccode.repo /etc/yum.repos.d/
sudo yum update
sudo yum -y upgrade
sudo yum install -y perl-Tk perl-PDL pgplot pgplot-devel gnuplot libtk-codetext-perl
cd /home/vagrant
sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1
sudo -u vagrant rpmbuild --rebuild McCode/support/rpm/SRPMS/perl-ExtUtils-F77-1.16-5.el6.src.rpm
sudo rpm -i rpmbuild/RPMS/noarch/perl-ExtUtils-F77-1.16-5.el7.centos.noarch.rpm
sudo -u vagrant rpmbuild --rebuild McCode/support/rpm/SRPMS/perl-PGPLOT-2.21-5.src.rpm
sudo rpm -i rpmbuild/RPMS/x86_64/perl-PGPLOT-2.21-5.x86_64.rpm
cd McCode
sudo -u vagrant ./build_rpms_mcstas 2.5beta01 meta
rm dist/mcstas-tools-matlab-mcplot*.rpm
sudo rpm -i dist/*.rpm


#./build_rpms_mcstas test

#sudo apt-get install -y vim build-essential
#sudo apt-get -y install python-pip
#sudo apt-get -y install python-sphinx
#sudo pip install sphinx_rtd_theme
 
#sudo apt-get install dpkg-dev fakeroot lintian

# Now with mpistuff 
#sudo apt-get install -y openmpi-bin openmpi-doc libopenmpi-dev

#Install viewer to nexus files 
#sudo apt-get install -y hdfview

# Install PyCharm community edition
#sudo add-apt-repository ppa:mystic-mirage/pycharm
#sudo apt-get update
#sudo apt-get install -y --force-yes pycharm-community 

