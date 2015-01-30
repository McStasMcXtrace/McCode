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
DN="dc=${DN//./,dc=}"               # replacing '.' with ',dc=' and putting dc= in front.
echo Obtained LDAP database DN: $DN
echo " "
#---------------#
# Get passwords #
#---------------#
count=0
warn="."
RPW1="."
until [[ $RPW1 == $RPW2 ]]; do
    echo "Please input your LDAP root password"$warn
    read -s RPW1
    echo "Please repeat the password"$warn
    read -s RPW2
    warn=", ensure passwords match."
    echo " "
done
ROOTPW=`/usr/sbin/slappasswd -s $RPW1`
warn="."
BPW1="."
until [[ $BPW1 == $BPW2 ]]; do
    echo "Please input your moodle bind password"$warn
    read -s BPW1
    echo "Please repeat the password"$warn
    read -s BPW2
    warn=", ensure passwords match."
    echo " "
done
BINDPW=`/usr/sbin/slappasswd -s $BPW1`
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

#---------------------#
# Calling DB builders #
#---------------------#
python manage.py syncdb
python ./bin/ldap-build.py $DN $ROOTPW $BINDPW $TREEPW $RPW1

