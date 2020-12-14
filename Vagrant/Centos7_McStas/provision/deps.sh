#! /bin/bash

# Install basic mccode dependencies

curl -O http://packages.mccode.org/rpm/mccode.repo
sudo cp mccode.repo /etc/yum.repos.d/
sudo yum update
sudo yum -y upgrade
sudo yum install -y perl-Tk perl-PDL pgplot pgpplot-dev libtk-codetext-perl git
cd /home/vagrant

sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1 --recurse-submodules -b mcstas-3.0

sudo -u vagrant rpmbuild --rebuild McCode/support/rpm/SRPMS/perl-ExtUtils-F77-1.16-5.el6.src.rpm
sudo rpm -i rpmbuild/RPMS/noarch/perl-ExtUtils-F77-1.16-5.el7.noarch.rpm
sudo -u vagrant rpmbuild --rebuild McCode/support/rpm/SRPMS/perl-PGPLOT-2.21-5.src.rpm
sudo rpm -i rpmbuild/RPMS/x86_64/perl-PGPLOT-2.21-5.x86_64.rpm

sudo -u vagrant mkdir -p McCode/dist
sudo -u vagrant cp rpmbuild/RPMS/x86_64/perl-PGPLOT-2.21-5.x86_64.rpm McCode/dist
sudo -u vagrant cp rpmbuild/RPMS/noarch/perl-ExtUtils-F77-1.16-5.el7.noarch.rpm McCode/dist

cd /home/vagrant/McCode
sudo -u vagrant ./getdeps_centos7


