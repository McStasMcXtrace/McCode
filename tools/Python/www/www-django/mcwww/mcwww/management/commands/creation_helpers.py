#=================================#
# Helper functions for createuser #  ADD ERRORHANDLING FOR LDAP ERRORS INSTEAD OF LOGIC PLS
# and createsuperuser             #  
# -------------------             #
# Author: Mark Lewis              #
#=================================#
#----------------#
# python imports #
#----------------#
import getpass
from subprocess import Popen, PIPE
#----------------#
# django imports #
#----------------#
from django.utils.six.moves import input
from django.utils.encoding import force_str
from django.core import exceptions
from django.contrib.auth.models import User, Group
#-------------#
# app imports #
#-------------#
# from mcUser.management.LDAP.LDAPComm import *
# from mcUser.models import *
#=============================#
# makeuid                     #
# -------                     #
# Makes a 5 char uid from the #
# passed username             #
#=============================#
def makeuid(username, n=0):
    retname = ""
    if n==0:
        retname = username[:4]
    elif len(username) > 5:
        retname = username[:3] + str(n-1)
    return retname
#==================================#
# get_cn                           #
# ------                           #
# queries user for their LDAP      #
# credentials and offers a default #
# option.                          #
# Returns the captured cn.         #
#==================================#
def get_cn():
    default = 'cn=admin,dc=nodomain'
    print "{USING default_dn: %s}"% default
    return default
    #--------------------------------#
    # PUT THIS BACK IN ONCE THE LDAP #
    # GROUP ADMIN AUTH CHECK THING   #
    # WORKS!                         #
    #--------------------------------#
    #    LDAP_admin_cn = raw_input('Enter your LDAP authentication username (cn=...,dc=nodomain)\n (enter for default): ')
    #    if not LDAP_admin_cn: 
    #        return default
    #    if check_LDAP_perms(LDAP_admin_cn):
    #        return LDAP_admin_cn
#====================================================#
# duplicate_user_check                               #
# --------------------                               #
# Queries sqlite db for duplicate users              #
# and assigns usr_details['uid'] to an allowed value #
#====================================================#
def duplicate_user_check(usr_details):
    uid_n = 0
    while True:
        usr_details['uid'] = makeuid(usr_details['username'], uid_n)
        if mcUser.objects.filter(uid=usr_details['uid']).count() > 0:
            print "\n  Username %s already exists, exiting.\n"% usr_details['username']
            sys.exit(1)
        else: return True
#==================================================#
# encrypt_password                                 #
# ----------------                                 #
# assigns SSHA password to usr_details['password'] #
# using LDAP daemons slpapasswd                    #
#==================================================#
def encrypt_password(usr_details):
    fid = Popen(["slappasswd", "-s", usr_details['password']],
                stdout=PIPE,
                stderr=PIPE)
    stdout,stderr = fid.communicate()
    if "{SSHA}" in stdout: usr_details['password'] = stdout
    else:                                                                  # make and throw LDAPpwdCreationError
        print "Error in password creation."
        sys.exit(1)
#=========================================#
# get_email                               #
# ---------                               #
# - uses the django field to ensure email #
#  is correctly formatted                 #
#=========================================#
def get_email(usr_details):
    usr_details['email'] = None
    while not usr_details['email']:
        email = input(force_str('email: '))
        try:
            #            usr_details['email'] = mcUser._meta.get_field('email').clean(email, None)
            usr_details['email'] = User._meta.get_field('email').clean(email, None)
        except exceptions.ValidationError as e:
            print "Error: %s" % '; '.join(e.messages)
            usr_details['email'] = None
#============================================#
# check_LDAP_perms                           #
# ----------------                           #
# Requests admin information fron the user   #
# and tests to see if these have admin group #
# permissions ussing the LDAPComm object     #
#============================================#
def check_LDAP_perms(cn):
    comm = LDAPComm.LDAPComm()
    if cn == 'cn=admin,dc=branch' :
        return True
    else:
        LDAP_admin_dn = "cn=%s,ou=person,dc=branch" % cn
        if(not comm.ldapAdminGroupQuery(LDAP_admin_dn)):     # make and throw InsufficientLDAPPrivsError
            print "Insufficient LDAP privs, your cn may not be what you have supplied.\nPlease contact admin.\n"
            sys.exit(1)
        return True


