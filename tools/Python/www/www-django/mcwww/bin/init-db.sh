#!/bin/sh
export PYTHONPATH=$PYTHONPATH:./

# PUT A .gitignore IN sim FOLDER.
# CHECK FOR SECURITY UPDATES IN DJANGO 1.7
# CHANGE ERROR MESSAGES
# BETTER PASSWORD PROMPTS - lable each pwd something simple 'level 1, 2, 3'
# READ ME FOR EACH .LDIF TEMPLATE
# TRY TO PUT THE SAME USERNAME PROMPT FOR LDAP AND SQLITE

# NB: $i = ith argument, $# = tot num args

#----------------------#
# Getting dependencies #
#----------------------#
sudo ./bin/get-dependencies.sh
sudo apt-get install python-dev python-pip sqlite3 # these can be moved to where get dependencies looks
sudo pip install django==1.7 jsonfield simplejson

#--------------------------------------#
# Refreshing DBs if they are installed #
#--------------------------------------#
if [ -f DB/mcwww.sqlite3 ] ; then
    rm DB/mcwww.sqlite3
fi

#-------------------------------#
# Running the simulation builds #
# Poulating the sqlite DB       #
#-------------------------------#
python manage.py syncdb
./bin/update-simulations.sh
# this is just for debug
./manage.py createuser mark 1