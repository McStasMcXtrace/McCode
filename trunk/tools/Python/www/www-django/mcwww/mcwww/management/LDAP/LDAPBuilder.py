# =======================#
# Build the McStasServer #
# LDAP authentication DB #
# ---------------------- #
# Author: Mark Lewis     #
#========================#
from LDAPComm import *
from LDAPData import LDAPData
from LDAPldiffer import addUserLDIF

# LDAPBuilder class 
# =================
class LDAPBuilder:
    def __init__(self, data):
# DB access and data
        self.pipe = LDAPComm()
        self.access = LDAPData()
        self.buildWith = data
# DB modification files
        self.ldif_build_path = './mcwww/management/LDAP/LDIFs/temp/'
        self.rootPW = "RPW.ldif"
        self.tree = "DIT.ldif"
        self.acl = "ACL.ldif"
# ROOTPW insertion (Backend Modification)
    def insertRootPW(self):
        print "adding olcRootPW to olcDatabase={0}config"
        ldif_file = self.ldif_build_path + self.rootPW
        self.pipe.ldapSYSROOTmod(ldif_file)
        print "Done. If successful please:"
        print "REMEMBER TO REMOVE THE olcRootPW DIRECTIVE WHEN ALL {0} OR config DB MANIPULATION IS DONE."

# Build DIT (Frontend Modification).
    def buildTree(self):
        print "Building tree in olcDatabase={1}hdb"
        ldif_file = self.ldif_build_path + self.tree
        print self.buildWith.tree_dn()
        self.pipe.ldapAdd(ldif_file, self.buildWith.tree_dn(), self.buildWith.tree_pw())

# Set up group access control (Backend Modification).
    def buildAcl(self):
        print "Building access control in olcDatabase={1}hdb"
        ldif_file = self.ldif_build_path + self.acl
        self.pipe.ldapAdd(ldif_file, self.buildWith.root_dn(), self.buildWith.root_pw())

# User population : may/may not be necc (Frontend Modification).
    def insertPopulation(self):
        print "Adding Users to olcDatabase={1}hdb"
        ldif_file = self.ldif_build_path + self.users
        self.pipe.ldapAdd(ldif_file, self.buildWith.tree_dn(), self.buildWith.tree_pw())
