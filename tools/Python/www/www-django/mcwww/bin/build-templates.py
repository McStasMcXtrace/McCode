#============================#
# build-templates.py         #
# ------------------         #
# Templated file manipulator #
#============================#
def main(args):
    # - dirs and kwds
    kwds = {'DN':args[1], 'RPW':args[2],'BPW':args[3]}
    in_dirs  = ['./mcUser/management/LDAP/templates/LDIFs/',     
                './mcUser/management/LDAP/templates/LDAPython/',
                './mcUser/management/commands/templates/',
                './mcUser/templates/']
    out_dirs = ['./mcUser/management/LDAP/LDIFs/',
                './mcUser/management/LDAP/',
                './mcUser/management/commands/',
                './mcUser/',]
    # - Replacing keywords
    for i in range(4):
        for in_file in os.listdir(in_dirs[i]):
            if '.py' in in_file or '.ldif' in in_file:
#                print "Replacing keywords in :", in_dirs[i], in_file
                outfile = open('%s%s'%(out_dirs[i],in_file),'w')
                for line in open("%s%s"%(in_dirs[i],in_file), 'r'):
                    for (kwd, replacement) in kwds.items():
                        line = line.replace(kwd,replacement)
                    outfile.write(line)
                outfile.close()


main(sys.argv)
