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


#---------#
# Helpers #
#---------#
def makeuid(username, n=0):
    retname = ""
    if n==0:
        retname = username[:4]
    elif len(username) > 5:
        retname = username[:3] + str(n-1)
    return retname

def usrCheck():
    def quitter():
        "\n Trying to avoid duplicates in the database:\n     Please put \'y\' or \'n\' in next time.\n"
        sys.exit(1)
    input_dict = {'y': True,
                  'n': False,
                  'break': quitter()
                  }
    def inputCheck(n=0):
        if n >0: print "Input not understood:\n   "
        chk = raw_input("Is this your user? (y/n)") 
        for key in input_dict.keys():
            if chk == key: return chk
            if n > 10: return 'break'
        return inputCheck(count+1)
    return input_dict.get(inputCheck(), 'Input not caught')

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
    comm = LDAPComm()
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

    print "\nBefore LDAPData population: ", usr_details,"\n"

    #======================#
    # LDAP DB Modification #
    # --------------------
    #
    # - Check permissions
    LDAP_admin_cn = raw_input('Enter your LDAP authentication cn (not your uid): ')
    LDAP_admin_pw = getpass('Enter your LDAP authentication pwd: ')
    if LDAP_admin_cn == 'cn=admin,dc=fysik,dc=dtu,dc=dk' :
        LDAP_admin_dn = LDAP_admin_cn
    else:
        LDAP_admin_dn = "cn=%s,ou=person,dc=fysik,dc=dtu,dc=dk" % LDAP_admin_cn
        if(not comm.ldapAdminGroupQuery(LDAP_admin_cn)): 
            print "Insufficient LDAP privs, your cn may not be what you have supplied.\nPlease contact admin.\n"
            sys.exit(1)
    # - Check for duplicates
    uid_n = 0
    usr_details['uid'] = ""
    while True:
        usr_details['uid'] = makeuid(usr_details['username'], uid_n)
        if mcUser.objects.filter(uid=usr_details['uid']).count() > 0:
            print mcUser.objects.filter(uid=usr_details['uid']), "\n\n"
            if usrCheck():
                print "\n  User already exists, exiting.\n"
                sys.exit(1)
        else: break
    # - Set pwd
    fid = Popen(["slappasswd", "-s", usr_details['password']],
                stdout=PIPE,
                stderr=PIPE)
    stdout,stderr = fid.communicate()
    if "{SSHA}" in stdout: usr_details['password'] = stdout
    else: 
        print "Error in password creation."
        sys.exit(1)
    # - LDAPUserCreation call
    entity = LDAPUserCreation(usr_details)
    print "Calling: processLDIF(", LDAP_admin_dn, ",", LDAP_admin_pw, ")"
    entity.processLDIF(LDAP_admin_dn, LDAP_admin_pw)                                             # Nice to report success or not (later! get it comitted!!)
    print "LDAP User Added"
    #
    # ---------------------
    # End LDAP Modification #
    #=======================#
    
    print "\nAfter LDAPData population: ", usr_details,"\n"

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
