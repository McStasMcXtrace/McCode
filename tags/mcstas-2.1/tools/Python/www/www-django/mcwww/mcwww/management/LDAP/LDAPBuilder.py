# Build the McStasServer LDAP DB from an empty DB

# Home grown imports
from LDAPComm import LDAPComm
from LDAPData import LDAPData
from LDAPldiffer import addUserLDIF

# LDAPBuilder class 
# =================
# init:
#   Connection to LDAP DB: UNDER CONSTRUCTION
#   LDAP access data: Populate from the __init__ functrion of the calling user (in the future)
#   DB structure: Include a new objectClass when OID comes
#   DB access: Properly configure for groups
#   Temporary User Data : Properly configure for std courseStaff and itStaff inclusion (Maybe Peter and Linda are good choices for contacts.)
class LDAPBuilder:
    def __init__(self):
# DB access methods and data
        self.pipe = LDAPComm()
        self.access = LDAPData()
        self.buildWith = BuildData()
# DB creation files
        self.ldif_build_path = 'LDIFs/building/'
        self.rootPW = "ROOTPW.ldif"
        self.tree = "DITbuild.ldif"
        self.acl = "ACLbuild.ldif"
        self.users = "INIT_POPbuild.ldif"

# puts the most powerful user (access to ALL_DB_AND_BE.* by THIS_GUY modify) in DB={0}config.ldif (Config DB modification).
    def insertRootPW(self):
        print "adding olcRootPW to olcDatabase={0}config"
        ldif_file = self.ldif_build_path + self.rootPW
        self.pipe.ldapSYSROOTmod(ldif_file)
        print "Done. If successful please:"
        print "REMEMBER TO REMOVE THE olcRootPW DIRECTIVE WHEN ALL {0} OR config DB MANIPULATION IS DONE."

# Tree Structure: builds the DIT (Frontend DB modification).
    def buildTree(self):
        print "Building tree in olcDatabase={1}hdb"
        ldif_file = self.ldif_build_path + self.tree
        print self.buildWith.tree_dn()
        self.pipe.ldapAdd(ldif_file, self.buildWith.tree_dn(), self.buildWith.tree_pw())

# Access Control: sets up the group accesses and indexes search terms (Frontend DB modification).
    def buildAcl(self):
        print "Building access control in olcDatabase={1}hdb"
        ldif_file = self.ldif_build_path + self.acl
        self.pipe.ldapAdd(ldif_file, self.buildWith.root_dn(), self.buildWith.root_pw())

# User population : this may not be necc to ship with the completed system (Frontend DB modification).
    def insertPopulation(self):
        print "Adding Users to olcDatabase={1}hdb"
        ldif_file = self.ldif_build_path + self.users
        self.pipe.ldapAdd(ldif_file, self.buildWith.tree_dn(), self.buildWith.tree_pw())

class BuildData:
    def __init__(self):
        self.treedn = 'cn=admin,dc=fysik,dc=dtu,dc=dk'
        self.treepw = 'traumaroot'
        self.rootdn = 'cn=admin,cn=config'
        self.rootpw = 'traumaroot'
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
