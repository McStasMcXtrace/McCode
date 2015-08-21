#============================#
# build-templates.py         #
# ------------------         #
# Templated file manipulator #
#============================#
import sys
import os
import re
def main(args):
    #-------------------------#
    # setup ACL.ldif template #
    #-------------------------#
    access_lines = []
    check_for_more = False
    current_line = ""
    # get old access lines
    with open('./mcUser/management/LDAP/templates/olcDatabase={1}hdb.ldif', 'r') as ACL_file:
        for line in ACL_file.readlines():
            if check_for_more:
                if line[0] == " ": current_line = current_line[:-1] + line[1:]
                else             : check_for_more = False
            if current_line and not check_for_more:
                check_for_more = False
                access_lines.append(current_line)
                current_line = ""
            if 'olcAccess:' in line:
                check_for_more = True
                current_line = line
    # changing for insertion to ACL.ldif
    for i in range(len(access_lines)):
        access_lines[i] = re.sub("{\d}", "", access_lines[i])
        access_lines[i] = re.sub(' by', "\n  by", access_lines[i])
    # building ACL template
    del_chunk = open('./mcUser/management/LDAP/templates/ACL_del.ldif','r').readlines()
    with open('./mcUser/management/LDAP/templates/LDIFs/ACL_del.ldif', 'w') as ACL_file:
        for line in del_chunk:
            ACL_file.write(line)
        for line in access_lines:
            ACL_file.write('delete: olcAccess\n')
            ACL_file.write(line)
            ACL_file.write('-\n')
    #-------------------------#
    # Replacing template kwds #
    #-------------------------#
    # dirs and kwds
    kwds = {'D_N':args[1], 'RPW':args[2],'BPW':args[3]}
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
                outfile = open('%s%s'%(out_dirs[i],in_file),'w')
                for line in open("%s%s"%(in_dirs[i],in_file), 'r'):
                    for (kwd, replacement) in kwds.items():
                        line = line.replace(kwd,replacement)
                    outfile.write(line)
                outfile.close()
main(sys.argv)
