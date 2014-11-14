#!/bin/sh

DN=`sed '2q;d' /etc/hosts`
NAME=`cat /etc/hostname`
DN=${DN%$NAME*}     # all before (repeated) NAME
DN=${DN##*$NAME.}   # get after first NAME on line

echo "Please input your LDAP root password"
ROOTPW=`slappasswd`
echo "Please input your moodle bind password"
BINDPW=`slappasswd`
echo "Please input your LDAP DB admin password (the one you set on slapd install)"
read TREEPW

echo ROOTPW cipher: $ROOTPW
echo BINDPW cipher: $BINDPW
echo TREEPW : $TREEPW
echo name: $NAME
echo dn: $DN

#python ldap-build.py
#python manage.py syncdb
