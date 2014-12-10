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
    def __init__(self):
# DB access and data
        self.pipe = LDAPComm()
        self.access = LDAPData()
        self.buildWith = BuildData()
# DB modification files
        self.ldif_build_path = 'LDIFs/building/'
        self.rootPW = "ROOTPW.ldif"
        self.tree = "DITbuild.ldif"
        self.acl = "ACLbuild.ldif"
        self.users = "INIT_POPbuild.ldif"
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

class BuildData:
    def __init__(self):
        self.treedn = 'cn=admin,dc=<MACHINE DOMAIN>'
        self.treepw = '<ADMINPW>'
        self.rootdn = 'cn=admin,cn=config'
        self.rootpw = '<ROOTPW>'
        print "build Data op"
        print self.treedn
        print self.rootdn
        print self.treepw
        print self.rootpw
    def tree_dn(self):
        return self.treedn
    def tree_pw(self):
        return self.treepw
    def root_dn(self):
        return self.rootdn
    def root_pw(self):
        return self.rootpw

bob = LDAPBuilder()
bob.insertRootPW()
bob.buildTree()
bob.buildAcl()
bob.insertPopulation()
