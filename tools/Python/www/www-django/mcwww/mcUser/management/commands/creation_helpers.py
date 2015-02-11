#=================================#
# Helper functions for createuser #
# and createsuperuser             #
# -------------------             #
# Author: Mark Lewis              #
#=================================#
from mcUser.management.LDAP.LDAPComm import *

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

def checkLDAPperms():
    LDAP_admin_cn = raw_input('Enter your LDAP authentication cn (not your uid): ')
    LDAP_admin_pw = getpass('Enter your LDAP authentication pwd: ')
    if LDAP_admin_cn == 'cn=admin,dc=branch' :
        LDAP_admin_dn = LDAP_admin_cn
    else:
        LDAP_admin_dn = "cn=%s,ou=person,dc=branch" % LDAP_admin_cn
        if(not comm.ldapAdminGroupQuery(LDAP_admin_cn, LDAP_auth_pw)): 
            print "Insufficient LDAP privs, your cn may not be what you have supplied.\nPlease contact admin.\n"
            sys.exit(1)


