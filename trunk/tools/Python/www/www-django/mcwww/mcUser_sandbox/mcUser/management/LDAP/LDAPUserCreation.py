#=====================================================#
# LDAPUserCreation                                    #
# ----------------                                    #
# Simple 'fire and forget' class to build LDAP DB     #
# modification user ldifs.                            #
# ------------------------                            #
# - __init__ calls the methods sequentially           #
#   and kills everything.                             #
# - returns true if LDAP user is added, returns false #
#   if LDAP user is not added.                        #
# - THIS NEEDS TO BE MADE INTO A VIEW FOR USE IN THE  #
#   ADMIN WEBPAGE                                     #
# ------------------                                  #
# Author: Mark Lewis                                  #
#=====================================================#
from LDAPComm import * 
from LDAPData import *
from LDAPldiffer import addUserLDIF
#================================================#
# __init__                                       #
# ---------                                      #
# - Prompts user admin for credentials           #
# - Instantiate LDAPData object                  #
# - Tests credentials against LDAP groups        #
# - Calls other methods in class                 #
#                                                #
# collectData                                    #
# -----------                                    #
# - Prompts the admin user for the user data     # 
# - Saves input in LDAPData object.              #
#                                                #
# processLDIF                                    #
# -----------                                    #
# - Calls LDAPComm.ldapAdd with addUserLdif file #
# and credentials given by the admin user.       #
#================================================#
class LDAPUserCreation:
    def __init__(self, usr_details):
        def collectData():
            FN   = raw_input('Enter Forename         : ')
            SN   = raw_input('Enter Surname          : ')
            G    = raw_input('Is user staff (y/n)?   : ')
            MAIL = raw_input('Enter Contact mail     : ')
            self.ldap_user.setnames(FN, SN)
            self.ldap_user.setmail(MAIL)
            if G == 'y':
                usr_details['staff'] = True
                self.ldap_user.setgroup('courseStaff')
            else: usr_details['staff'] = False
            usr_details['username']    = self.ldap_user.cn()
            usr_details['email']       = MAIL
            usr_details['displayname'] = FN


        self.comm = LDAPComm()
        self.ldap_user = LDAPData()
        self.ldap_user.setuid(usr_details['uid'])
        self.ldap_user.setpassword(usr_details['password'])
        self.ldap_user.setldif_file("./mcUser/management/LDAP/LDIFs/temp/" + usr_details['uid'] + ".ldif")
        collectData()
        addUserLDIF(self.ldap_user)


    def processLDIF(self, dn, pw):
        self.comm.ldapAdd(self.ldap_user.ldif(), dn, pw)
