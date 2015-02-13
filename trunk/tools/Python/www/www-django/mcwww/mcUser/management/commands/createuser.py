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
from creation_helpers import makeuid, check_LDAP_perms, duplicate_user_check, encrypt_password

#==================================
# User creation                                    TODO LIST
# -------------                                    ---------
# - Get user name from command line
# - Check for user id in sqlitedb              <-- need to change this to UID - DONE
# - Get password from cmd line or
#   request input                              <-- slappwd this bit
# - Make LDAP modifications (working)
# - Write new user information to sqlitebd     <-- need to access mcUserDB instead NB: no password to be stored in sqlite DB
# - Save user in sqlitedb      
# - Add group                                  <-- should do this first for entry to LDAP
def main(args):
    comm = LDAPComm.LDAPComm()
    usr_details = {}
    #-----------------------#
    # username and password #
    #-----------------------#
    if len(args) < 1:
        print 'usage: createuser username [password]'
        print "GIVEN ARGUMENTS: ", args
        sys.exit(1)
    usr_details['username'] = args[0]
    print 'Username:', usr_details['username']
    # Getting user password
    if len(args) > 1:
        print 'Password taken from argument'
        usr_details['password'] = args[1]
    else:
        usr_details['password'] = getpass('Enter password: ')    
    #======================#
    # LDAP DB Modification #
    # --------------------
    #
    check_LDAP_perms()
    usr_details['uid'] = ""
    # - Check for duplicates
    duplicate_user_check(usr_details)
    # - Set pwd
    encrypt_password(usr_details)
    # - LDAPUserCreation call
    entity = LDAPUserCreation(usr_details)
    print "Calling: processLDIF(", LDAP_admin_dn, ",", LDAP_admin_pw, ")"
    entity.processLDIF(LDAP_admin_dn, LDAP_admin_pw)                                             # Nice to report success or not (later! get it comitted!!)
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
    
    # Add to groups implied in the args
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

    help = "Whatever you want to print here"

    def handle(self, *args, **options):
        main(args)
