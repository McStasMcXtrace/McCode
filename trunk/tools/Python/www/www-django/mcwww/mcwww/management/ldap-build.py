#==============================#
# LDAP Authentication DB Setup #
#==============================#
import sys, os
from sys import stdin
import management.LDAP.LDAPBuilder as bob_the # <---------- need this import to work from .../management/commands/

def main(args):
    if len(args) < 5:
        print  "usage: python ldap-build.py <dn> <rootpw> <bindpw> <your pw>\n"
        print "Not enough args: "
        print "GIVEN ARGUMENTS: ",args,"\n"
        sys.exit(1)
    
#    print "Arguments Supplied: ",args,"\n"
    
    '''
    out_dir = 'management/LDAP/LDIFs/temp'               # These paths must be resolved properly from the calling class
    ldif_dir = 'management/LDAP/LDIFs/'                  # ... see the sandbox mcUser
    '''

    out_dir = '../management/LDAP/temp/'
    ldif_dir = '../management/LDAP/LDIFs/'

    kwds = {'DN':args[1], 'RPW':args[2],'BPW':args[3]}
    for ldif in os.listdir(ldif_dir):
        print "ldif: ",ldif
        if '.ldif' in ldif:
            print "dir:",out_dir," file:", ldif
            outfile = open('%s%s'%(out_dir,ldif),'w')
            for line in open("%s%s"%(ldif_dir,ldif), 'r'):
                for (kwd, replacement) in kwds.iteritems():
                    line = line.replace(kwd,replacement)
                outfile.write(line)
            outfile.close()
#    print "Data:",args[1],args[4],args[2]
    dat = BuildData(args[1], args[4], args[2])
    # auth = bob_the.LDAPBuilder(dat)
    # auth.insertRootPW()
    # auth.buildTree()
    # auth.buildAcl()



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



