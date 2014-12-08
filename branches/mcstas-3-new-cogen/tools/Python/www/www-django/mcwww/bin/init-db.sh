#!/bin/sh
export PYTHONPATH=$PYTHONPATH:./
#------------------------------------------#
# Get host name for populating .ldif files #
#------------------------------------------#
DN=`sed '2q;d' /etc/hosts`
NAME=`cat /etc/hostname`
DN=${DN%$NAME*}                     # all before (repeated) NAME
DN=${DN##*$NAME.}                   # get after first NAME on line 
DN=$(echo $DN | sed 's/ *$//')      # trim whitespace at end
DN="${DN//./,dc=}"               # replacing '.' with ',dc='
echo Obtained LDAP database DN: $DN
echo " "
#---------------#
# Get passwords #
#---------------#
count=0
warn="."
until [[ ${ROOTPW:0:1} == "{" ]]; do
    echo "Please input your LDAP root password"
    read -s PROOTPW
    ROOTPW=`slappasswd -s $PROOTPW`
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
    LDAPOP=`ldapwhoami -D cn=admin,dc=$DN -w $TREEPW`
done
echo Access by admin accepted.
echo " "

#---------------------#
# Calling DB builders #
#---------------------#
python ./bin/ldap-build.py $DN $ROOTPW $BINDPW $TREEPW $PROOTPW
#python manage.py syncdb
