# =======================#
# Build the McStasServer #
# LDAP authentication DB #
# ---------------------- #
# Author: Mark Lewis     #
#========================#
import os

from mcUser.management.LDAP.LDAPComm import *
from mcUser.management.LDAP.LDAPData import LDAPData
from mcUser.management.LDAP.LDAPldiffer import addUserLDIF

# LDAPBuilder class 
# =================
class LDAPBuilder:
    def __init__(self, data):
# DB access and data
        self.pipe = LDAPComm()
        self.access = LDAPData()
        self.buildWith = data
# DB modification files
        print "BUILDER IS RUNNING IN: ", os.getcwd()
        self.ldif_build_path = './mcUser/management/LDAP/LDIFs/'
        self.rootPW = "RPW.ldif"
        self.tree = "DIT.ldif"
        self.acl_del = "ACL_del.ldif"
        self.acl_add = "ACL_add.ldif"
        
# ROOTPW insertion (Backend Modification)
    def insertRootPW(self):
        print "Adding olcRootPW to olcDatabase={0}config.ldif"
        ldif_file = self.ldif_build_path + self.rootPW
        self.pipe.ldapSYSROOTmod(ldif_file)
        print "\nIf successful please:"
        print "REMEMBER TO REMOVE THE olcRootPW DIRECTIVE WHEN ALL {0} OR config DB MANIPULATION IS DONE."
        print "THIS MUST BE DONE MANUALLY - SEE INSTALL INSTRUCTIONS\n"

# Build DIT (Frontend Modification).
    def buildTree(self):
        print "Building tree in olcDatabase={1}hdb.ldif"
        ldif_file = self.ldif_build_path + self.tree
        print self.buildWith.tree_dn()
        self.pipe.ldapAdd(ldif_file, self.buildWith.tree_dn(), self.buildWith.tree_pw())

# Set up group access control (Backend Modification). modification: Verbose, Debug level 5.
# Two accesses to the database - acl_del removes teh olcAccess lines in olcDatabas{1}hdb.ldif ** this doesn't work **
#                              - acl_add adds the olcAccesslines in olcDatabase{0}config.ldif
    def buildAcl(self):
        print "Attemping access control removal in olcDatabase={1}hdb.ldif"
        ldif_file = self.ldif_build_path + self.acl_del
        self.pipe.ldapMod(ldif_file, self.buildWith.root_dn(), self.buildWith.root_pw())
        print "Adding access control to olcDatabase={1}hdb.ldif"
        ldif_file = self.ldif_build_path + self.acl_add
        self.pipe.ldapMod(ldif_file, self.buildWith.root_dn(), self.buildWith.root_pw())
        
# User population : may/may not be necc (Frontend Modification).
# You can rebuild your database by dumping the LDAP to .ldif
# ** AN LDAP DUMP TO LDIF FILES FOR REBUILDING YOUR DB FROM SCRATCH IS A NICE FUNCTION TO ADD TO THE LIBRARY **
    def insertPopulation(self):
        print "Adding Users to LDAP DB."
        ldif_file = self.ldif_build_path + self.users
        self.pipe.ldapAdd(ldif_file, self.buildWith.tree_dn(), self.buildWith.tree_pw())
