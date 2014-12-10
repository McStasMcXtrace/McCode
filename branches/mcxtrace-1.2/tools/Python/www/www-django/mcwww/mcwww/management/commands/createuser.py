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
from django.core.management.base import BaseCommand, make_option
from mcwww.management.LDAP.LDAPUserCreation import *
from mcwww.management.LDAP.LDAPComm import *
from django.contrib.auth.models import User, Group
from getpass import getpass

def main(args):
    comm = LDAPComm()
# Checking and creation of username
    if len(args) < 1:
        print 'usage: createuser username [password]'
        print "GIVEN ARGUMENTS: ", args
        sys.exit(1)
    username = args[0]
    print 'Username:', username
# Getting user password
    if User.objects.filter(username=username).count() > 0:
        print 'Abort: User already exists!'
        sys.exit(1)
    if len(args) > 1:
        print 'Password taken from argument'
        password = args[1]
    else:
        password = getpass('Enter password: ')

    #======================#
    # LDAP DB Modification #
    # Check permissions
    LDAP_admin_cn = raw_input('Enter your LDAP authentication cn (not your uid): ')
    if(not comm.ldapAdminGroupQuery(LDAP_admin_cn)): 
        print "Insufficient LDAP privs, your cn may not be what you input.\n Try putting in surname or contact admin.\n"
        sys.exit(1)
    '''
    REQUEST GROUP MEMBERSHIP HERE : secondary importance, all are students at the moment
    '''
    # LDAPUserCreation call
    entity = LDAPUserCreation(username, password)
    LDAP_admin_pw = getpass('Enter your LDAP authentication pwd: ')
    LDAP_admin_dn = "cn=%s,ou=person,dc=fysik,dc=dtu,dc=dk" % LDAP_admin_cn
    print "Calling: processLDIF(", LDAP_admin_dn, ",", LDAP_admin_pw, ")"
    entity.processLDIF(LDAP_admin_dn, LDAP_admin_pw)
    print "LDAP User Added"
    # End LDAP Modification #
    #=======================#

# McCode User creation
    user = User.objects.create_user(username, password=password)
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
