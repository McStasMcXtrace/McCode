# At the moment this is a simple 'fire and forget' class.
# __init__ calls the methods sequentially and then kills everything. 
# (hopefully this clears memory right away)
# returns true if LDAP user is added, returns false if LDAP user is not added.
from getpass import getpass
from mcwww.management.LDAP.LDAPComm import * 
from mcwww.management.LDAP.LDAPData import *
from mcwww.management.LDAP.LDAPldiffer import addUserLDIF

# Command Line useage :
# - admin site population of variables to be done in POST form.
# - possible sec issues with passing plaintext pws for the LDAP DB.
#
# __init__
# ---------
# Prompts user admin for credentials
# Instantiate LDAPData object
# Tests credentials against LDAP groups
# Calls other methods in class
#
# collectData
# -----------
# Prompts the admin user for the user data 
# - cmd line, got to make POST form for this to be populated from admin site.
# Saves input in LDAPData object.
#
# processLDIF
# -----------
# calls the LDAPComm.ldapAdd method with an addUserLdif file and the credentials given by
# the admin user.
class LDAPUserCreation:
    def __init__(self, un, pw):
        self.comm = LDAPComm()
        self.ldap_user = LDAPData(un)

        self.ldap_user.setpassword(pw)
        self.ldap_user.setldif_file("./temp/" + un + ".ldif")
        self.collectData()
        addUserLDIF(self.ldap_user)

    def collectData(self):
        FN = raw_input('Enter Forename : ')
        SN = raw_input('Enter Surname : ')
        MAIL = raw_input('Enter Contact mail : ')
        self.ldap_user.setnames(FN, SN)
        self.ldap_user.setmail(MAIL)

    def processLDIF(self, dn, pw):
        self.comm.ldapAdd(self.ldap_user.ldif(), dn, pw)
