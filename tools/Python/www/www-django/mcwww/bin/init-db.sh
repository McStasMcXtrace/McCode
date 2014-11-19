#!/bin/sh

#------------------------------------------#
# Get host name for populating .ldif files #
#------------------------------------------#
DN=`sed '2q;d' /etc/hosts`
NAME=`cat /etc/hostname`
DN=${DN%$NAME*}                     # all before (repeated) NAME
DN=${DN##*$NAME.}                   # get after first NAME on line 
DN=$(echo $DN | sed 's/ *$//')      # trim whitespace at end
DN="dc=${DN//./,dc=}"               # replacing '.' with ',dc='
echo Obtained LDAP database DN: $DN
echo " "
#---------------#
# Get passwords #
#---------------#
count=0
warn="."
until [[ ${ROOTPW:0:1} == "{" ]]; do
    echo "Please input your LDAP root password"$warn
    warn=", ensuring passwords match."
    ROOTPW=`slappasswd`
    echo " "
done
warn="."
until [[ ${BINDPW:0:1} == "{" ]]; do
    echo "Please input your moodle bind password"$warn
    warn=", ensuring passwords match."
    BINDPW=`slappasswd`
    echo " "
done
warn="."
until [[ ${LDAPOP:0:1} == "d" ]]; do
    echo "Please input your LDAP DB admin password"$warn
    warn=" (the one you set on slapd install)."
    read -s TREEPW
    echo " "
    echo Testing LDAP accesses.
    LDAPOP=`ldapwhoami -D cn=admin,$DN -w $TREEPW`
done
echo Access by admin accepted.
echo " "

#--------------#
# Output check #
#--------------#
echo ROOTPW cipher: $ROOTPW
echo BINDPW cipher: $BINDPW
echo TREEPW : $TREEPW
echo name: $NAME
echo $DN

#---------------------#
# Calling DB builders #
#---------------------#
python ldap-build.py $DN $ROOTPW $BINDPW $TREEPW
#python manage.py syncdb
