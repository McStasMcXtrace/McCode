#===================================================================#
# User Creation Script                                              #
# --------------------                                              #
# Creates sqlite and LDAP DB entries.                               #
# Django User and Combined Authentication for Moodle and Mediawiki  #
# under same pwd.                                                   #
# ---------------                                                   #
# - Submitted: 17:00 (ish) 15-7-14                                  #
# - Completed primary: 9-10-14                                      #
# - Secondary: Group Membership                                     #
#              McUser implementation (possibly not necc but nice)   #
#===================================================================#
import sys
from sys import stdin
from subprocess import Popen, PIPE
import re
from django.core.management.base import BaseCommand, make_option
#---------------------#
# Django user imports #
#---------------------#
from django.contrib.auth.models import User, Group
from getpass import getpass
#-------------#
# App imports #
#-------------#
from mcUser.management.LDAP.LDAPUserCreation import *
from mcUser.management.LDAP.LDAPComm import *
from mcUser.models import *
from creation_helpers import duplicate_user_check, encrypt_password, get_cn, get_email

#=================================#
# User creation                   #                TODO LIST
# -------------                   #                ---------
# Creates an sqlite and LDAP user #
#=================================#                - Add groups in both DBs
def main(args):
    usr_details = {'staff':False}
    n_args = len(args)
    #-----------------------#
    # username and password #
    #-----------------------#
    if n_args < 1 or n_args > 2:
        print 'usage: createuser username [password]'
        print "GIVEN ARGUMENTS: ", args
        sys.exit(1)
    usr_details['username'] = args[0]
    print 'Username:', usr_details['username']
    # Getting user password
    if len(args) > 1:
        print 'Password taken from argument: %s'%args[1]
        usr_details['password'] = args[1]
    else:
        usr_details['password'] = getpass('Enter password: ')    
    encrypt_password(usr_details)
    get_email(usr_details)
    #======================#
    # LDAP DB Modification #
    # --------------------
    #
    # - Check permissions
    LDAP_admin_cn = get_cn()
    LDAP_admin_pw = getpass('Enter your LDAP authentication pwd: ')
    # - Check for duplicates
    duplicate_user_check(usr_details)
    # - LDAPUserCreation call
    entity = LDAPUserCreation(usr_details)
    print "Calling: processLDIF(", LDAP_admin_cn, ",", LDAP_admin_pw, ")"
    entity.processLDIF(LDAP_admin_cn, LDAP_admin_pw)                                             # Nice to report success or not (later! get it comitted!!)
    print "LDAP User Added"
    #
    # ---------------------
    # End LDAP Modification #
    #=======================#
    
    #======================#
    # McCode User creation #
    #======================#
    user = mcUser.objects.createMcUser(usr_details)
    print "\nMCUSER: ", user, "\n\n"
    user.save()
    #==================================#
    # This part should be done better. #
    #==================================#
    if len(args) > 2:
        for idx in range(2,len(args)):
            print '- adding user to group '+args[idx]
            try:
                g = Group.objects.get(name=args[idx]) 
                g.user_set.add(user)
            except:
                print 'Error: Group '+args[idx]+' does not exist!'
    print 'McCode User created.'

class Command(BaseCommand):
    username = 'username'
    password = 'password'
    help = "Help response\n-------------\nUsage: ./manage.py createuser username [password]"
    def handle(self, *args, **options):
        main(args)
