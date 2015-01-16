#==============================#
# LDAP Authentication DB Setup #
#==============================#
import sys, os
from sys import stdin

def main(args):
    if len(args) < 6:
        print  "usage: python ldap-build.py <dn> <rootpw> <bindpw> <your pw>\n"
        print "Not enough args: "
        print "GIVEN ARGUMENTS: ",args,"\n"
        sys.exit(1)
    print args
    #============================#
    # Templated file manipulator #
    # - dirs and kwds
    kwds = {'DN':args[1], 'RPW':args[2],'BPW':args[3]}
    in_dirs  = ['./mcUser/management/LDAP/templates/LDIFs/',     
                './mcUser/management/LDAP/templates/LDAPython/',
                './mcUser/management/commands/templates/']
    out_dirs = ['./mcUser/management/LDAP/LDIFs/',
                './mcUser/management/LDAP/',
                './mcUser/management/commands/']
    exts     = ['.ldif', '.py', '.py']
    # - Replacing keywords
    for i in range(3):
        for in_file in os.listdir(in_dirs[i]):
            if exts[i] in in_file:
                print "Replacing keywords in :", in_dirs[i], in_file
                outfile = open('%s%s'%(out_dirs[i],in_file),'w')
                for line in open("%s%s"%(in_dirs[i],in_file), 'r'):
                    for (kwd, replacement) in kwds.iteritems():
                        line = line.replace(kwd,replacement)
                    outfile.write(line)
                outfile.close()
    # Templates done             #
    #============================#
    #=====================#
    # Build LDAP database #
    from mcUser.management.LDAP import LDAPBuilder as bob_the
    print "Data:",args[1],args[4],args[5]
    dat = BuildData(args[1], args[4], args[5])
    auth = bob_the.LDAPBuilder(dat)
    auth.insertRootPW()
    auth.buildTree()
    auth.buildAcl()
    # Built               #
    #=====================#

#======================================#
# BuildData                            #
# ---------                            #
# Temporarily holds the root and admin #
# passwords                            #
#======================================#
class BuildData:
    def __init__(self, dn, treepw, rootpw):
        self.treedn = 'cn=admin,dc=%s'%dn
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
#   argv[0] =  'ldap-build.py'              #
#   argv[1] = LDAP DN                       #
#   argv[2] = LDAP Backend DB pw            #
#   argv[3] = moodle/mediawiki bind user pw #
#   argv[4] = LDAP Frontend DB  pw          #
#-------------------------------------------#
main(sys.argv)



