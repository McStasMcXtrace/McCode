#==========================================================#
# LDAPldiffer Module                                       #
# ------------------                                       #
# Creates various LDIF formatted files for McXtrace/McStas #
# web LDAP authenticate DB modification.                   #
# All fire and forget classes.                             #
# -------------------------------------------------------- #
# THIS MODULE NEEDS TO BE UPDATED WHEN THE DBs ARE BUILT!! # <-----!!! do this
#==========================================================#
from LDAPData import *
#===================================================#
# addUserLdif                                       #
# -----------                                       #
# Constructs LDIF formatted files from              #
# LDAPData object.                                  #
# -----------------                                 #
# - Builds a file to:                               #
#   - add an entry to the database in:              #
#       ou=person,dc=fysik,dc=dtu,dc=dk             #
#   - make the entry a member of the:               #
#       cn=Students,ou=groups,dc=fysik,dc=dtu,dc=dk #
#     group.                                        #
#===================================================#
class addUserLDIF:
    def __init__(self, ldap_user):
        out_file = open(ldap_user.ldif(), 'w')
        out_file.write("dn: cn=%s,ou=person,D_N\n" % ldap_user.cn())
        out_file.write("objectClass: inetorgperson\n")
        out_file.write("cn: %s \n" % ldap_user.cn())
        out_file.write("sn: %s \n" % ldap_user.sn())
        out_file.write("displayName: %s \n" % ldap_user.displayname)
        out_file.write("uid: %s \n" % ldap_user.uid())
        out_file.write("mail: %s \n" % ldap_user.mail())
        out_file.write("userpassword: %s \n" % ldap_user.password())
        out_file.write("ou: person\n")
#        out_file.write("memberof: cn=%s,ou=groups,D_N\n" % ldap_user.group()) # get this working for admin group permissions checking
        out_file.write("\n")
        out_file.write("dn: cn=%s,ou=groups,D_N\n" % ldap_user.group())
        out_file.write("changeType: modify\n")
        out_file.write("add: member\n")
        out_file.write("member: cn=%s,ou=person,D_N\n" % ldap_user.cn())
        out_file.close()
#=================================================#
# addUserLdif                                     #
# -----------                                     #
# Constructs LDIF formatted files from            #
# LDAPData object.                                #
# -----------------                               #
# - Builds a file to:                             #
#   - change a specific entry in the database in: #
#       ou=person,dc=fysik,dc=dtu,dc=dk           #
#     to replace the old pwd with a new one.      #
#=================================================#
class changepwLDIF:
    def __init__(self, ldap_user):
        out_file = open(ldap_user.ldif(), 'w')
        out_file.write("dn: cn=%s,ou=person,D_N\n" % ldap_user.cn())
        out_file.write("changetype: modify")
        out_file.write("replace: userpassword")
        out_file.write("userpassword: %s" % ldap_user.password())

