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
DN="${DN//./,dc=}"                  # replacing '.' with ',dc='
echo Obtained LDAP database DN: $DN
echo " "
#---------------#
# Get passwords #
#---------------#
ROOTPW=`slappasswd -s 7r4um4r00t`
BINDPW=`slappasswd -s spoons`
echo Access by admin accepted.
echo " "

#---------------------#
# Calling DB builders #
#---------------------#
echo python ./bin/ldap-build.py $DN $ROOTPW $BINDPW $TREEPW 7r4um4r00t 7r4um4r00t
python ./bin/ldap-build.py $DN $ROOTPW $BINDPW $TREEPW 7r4um4r00t 7r4um4r00t
python ./manage.py syncdb

