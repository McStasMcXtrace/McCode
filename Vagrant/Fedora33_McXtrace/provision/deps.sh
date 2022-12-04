#! /bin/bash

# Install basic mccode dependencies
sudo -sHu vagrant rpmbuild --rebuild McCode/support/rpm/SRPMS/perl-ExtUtils-F77-1.16-5.el6.src.rpm
sudo -sH rpm -i rpmbuild/RPMS/noarch/perl-ExtUtils-F77-1.16-5.el7.noarch.rpm
sudo -sHu vagrant rpmbuild --rebuild McCode/support/rpm/SRPMS/perl-PGPLOT-2.21-5.src.rpm
sudo -sH rpm -i rpmbuild/RPMS/x86_64/perl-PGPLOT-2.21-5.x86_64.rpm

sudo -u vagrant mkdir -p McCode/dist
sudo -u vagrant cp rpmbuild/RPMS/x86_64/perl-PGPLOT-2.21-5.x86_64.rpm McCode/dist
sudo -u vagrant cp rpmbuild/RPMS/noarch/perl-ExtUtils-F77-1.16-5.el7.noarch.rpm McCode/dist
