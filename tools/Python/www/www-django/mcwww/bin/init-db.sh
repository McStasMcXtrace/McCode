 #!/bin/sh
export PYTHONPATH=$PYTHONPATH:./

# PUT A .gitignore IN sim FOLDER.
# CHECK FOR SECURITY UPDATES IN DJANGO 1.7
# CHANGE ERROR MESSAGES
# BETTER PASSWORD PROMPTS - lable each pwd something simple 'level 1, 2, 3'
# READ ME FOR EACH .LDIF TEMPLATE
# TRY TO PUT THE SAME USERNAME PROMPT FOR LDAP AND SQLITE

# NB: $i = ith argument, $# = tot num args

# Need flags to direct DB builds
#   - Put into existing DB
#     - for this we need to scheck if olcRootDN and olcRootPW exist and to check if they are ok.
#   - Tear donw and rebuild
#   - Dump LDAP DB and rebuild
# - for all above options we need to change the DIT in the LDAPldiffer so that we have a McCode branch
#   so no possible interference with any existent DBs exist.



#--------------------------------------#
# Refreshing DBs if they are installed #
#--------------------------------------#
sudo apt-get install python-dev python-pip sqlite3
sudo pip install django==1.7 jsonfield
if [ -d /etc/ldap/slapd.d ]; then
    sudo apt-get purge -y ldap-utils slapd
    sudo apt-get install -y ldap-utils slapd
else
    sudo apt-get install -y ldap-utils slapd
fi
if [ -f DB/mcwww.sqlite3 ] ; then
    rm DB/mcwww.sqlite3
fi

# if [ -f mcUser/management/LDAP/LDAP_access/access.txt ] ; then
#    cat mcUser/management/LDAP/LDAP_access/access.txt >> mcUser/management/LDAP/LDAP_access/access.bak
#    rm mcUser/management/LDAP/LDAP_access/access.txt
# fi
#------------------------------------------#
# Get host name for populating .ldif files #
#------------------------------------------#
DN=`sudo slapcat | grep "dn: cn=admin" |cut -f2- -d,`
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
echo "Access by admin accepted."
echo "Getting LDAP auth structure."
sudo cp /etc/ldap/slapd.d/cn\=config/olcDatabase={1}hdb.ldif ./mcUser/management/LDAP/templates/
sudo chmod +r ./mcUser/management/LDAP/templates/olcDatabase={1}hdb.ldif
echo " "

#---------------------#
# Calling DB builders #
#---------------------#
python ./bin/build-templates.py $DN $ROOTPW $BINDPW
python ./bin/ldap-build.py $DN $ROOTPW $BINDPW $TREEPW $RPW1
python manage.py syncdb
python manage.py populate_db
