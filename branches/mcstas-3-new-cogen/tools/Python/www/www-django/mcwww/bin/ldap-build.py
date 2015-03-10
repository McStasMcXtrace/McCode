#==============================#
# LDAP Authentication DB Setup #
#==============================#
import sys, os
from sys import stdin

def main(args):
    if len(args) < 5:
        print  "usage: python 0:ldap-build.py 1:<dn> 2:<rootpw> 3:<bindpw> 4:<your pw>\n"
        print "Not enough args: "
        print "GIVEN ARGUMENTS: ",args,"\n"
        sys.exit(1)
    # Build LDAP database
    import LDAPBuilder as bob_the
    dat = BuildData(args[1], args[4], args[5])
    auth = bob_the.LDAPBuilder(dat)
    auth.insertRootPW()
    auth.buildTree()
    auth.buildAcl()

#======================================#
# BuildData                            #
# ---------                            #
# Temporarily holds the root and admin #
# passwords                            #
#======================================#
class BuildData:
    def __init__(self, dn, treepw, rootpw):
        self.treedn = 'cn=admin,%s'%dn
        self.treepw = treepw
        self.rootdn = 'cn=admin,cn=config'
        self.rootpw = rootpw
    def getDict(self):
        return {''}
    def tree_dn(self):
        return self.treedn
    def tree_pw(self):
        return self.treepw
    def root_dn(self):
        return self.rootdn
    def root_pw(self):
        return self.rootpw

#-------------------------------------------#
# Call to main with cmd line args:          #
#   args[0] =  'ldap-build.py'              #
#   args[1] = LDAP DN                       #
#   args[2] = LDAP Backend DB pw            #
#   args[3] = moodle/mediawiki bind user pw #
#   args[4] = LDAP Frontend DB PWD          #
#   args[5] = LDAP Admin PWD                #
#-------------------------------------------#
main(sys.argv)
