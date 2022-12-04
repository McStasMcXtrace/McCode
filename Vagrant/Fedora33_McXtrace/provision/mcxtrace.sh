#! /bin/bash

#git clone McCode
if [ ! -e McCode ]; then
        git clone --depth 1 --recursive https://github.com/McStasMcXtrace/McCode.git McCode
fi
cd McCode
git pull

#get runtime deps
./getdeps_fedora29

#get the extra dependencies F77 & PGPLOT
rpmbuild --rebuild support/rpm/SRPMS/perl-ExtUtils-F77-1.16-5.el6.src.rpm
sudo -sH rpm -i rpmbuild/RPMS/noarch/perl-ExtUtils-F77-1.16-5.el7.noarch.rpm
rpmbuild --rebuild support/rpm/SRPMS/perl-PGPLOT-2.21-5.src.rpm
sudo -sH rpm -i rpmbuild/RPMS/x86_64/perl-PGPLOT-2.21-5.x86_64.rpm

#build mcxtrace
./build_rpms_centos7_mcxtrace 1.5 yes

MINIC3RPM=`ls dist/mcxtrace*miniconda3*.rpm`
echo rpm -i $MINIC3RPM
sudo -sH rpm -i $MINIC3RPM

#install mcxtrace (minus metapackages)
for RPM in `ls -rt dist/mcxtrace*.rpm | grep -v suite|grep -v miniconda3`
do
        echo rpm -i $RPM
        sudo -sH rpm -i $RPM
done

