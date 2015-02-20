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
#-------------#
# app imports #
#-------------#
from mcUser.management.LDAP.LDAPComm import *
from mcUser.models import *
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
#===================================================#
# user_check                                        #
# ----------                                        #
# I'm pretty proud of the dictionary switch in this #
# so you can work out how it works yourself :p      #
#===================================================#
def user_check():
    def quitter():
        print "\n Trying to avoid duplicates in the database:\n"           # make this a UserAlreadyExistsError
        print "Please put \'y\' or \'n\' in next time.\n"
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
            print mcUser.objects.filter(uid=usr_details['uid']), "\n\n"
            if user_check():
                print "\n  User already exists, exiting.\n"
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
        raw_value = input(force_str('email: '))
        try:
            usr_details['email'] = mcUser._meta.get_field('email').clean(raw_value, None)
        except exceptions.ValidationError as e:
            self.stderr.write("Error: %s" % '; '.join(e.messages))
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
        LDAP_admin_dn = cn
    else:
        LDAP_admin_dn = "cn=%s,ou=person,dc=branch" % cn
        if(not comm.ldapAdminGroupQuery(LDAP_admin_dn)):     # make and throw InsufficientLDAPPrivsError
            print "Insufficient LDAP privs, your cn may not be what you have supplied.\nPlease contact admin.\n"
            sys.exit(1)


