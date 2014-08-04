from mcwww.management.LDAP.LDAPData import *
# Class to creater the various LDIF formatted files for McXtrace/McStas web 
#
# addUserLDIF
# -----------
# Takes an LDAPData object and constructs an LDIF formatted file
# to add an entry to the database in:
#    ou=person,dc=fysik,dc=dtu,dc=dk
# and make the entry a member of the:
#    cn=Students,ou=groups,dc=fysk,dc=dtu,dc=dk
# group.



# Makes an LDIF to add a user.
class addUserLDIF:
    def __init__(self, ldap_user):
        out_file = open(ldap_user.ldif(), 'w')
        out_file.write("dn: cn=%s,ou=Person,dc=fysik,dc=dtu,dc=dk\n" % ldap_user.cn())
        out_file.write("objectClass: inetorgperson\n")
        out_file.write("cn: %s \n" % ldap_user.cn())
        out_file.write("sn: %s \n" % ldap_user.sn())
        out_file.write("displayName: %s \n" % ldap_user.displayname())
        out_file.write("uid: %s \n" % ldap_user.uid())
        out_file.write("mail: %s \n" % ldap_user.mail())
        out_file.write("userpassword: %s \n" % ldap_user.password())
        out_file.write("ou: Person\n")
        out_file.write("\n")
        out_file.write("dn: cn=Student,ou=groups,dc=fysik,dc=dtu,dc=dk\n")
        out_file.write("changeType: modify\n")
        out_file.write("add: member\n")
        out_file.write("member: cn=%s,ou=Person,dc=fysik,dc=dtu,dc=dk\n" % ldap_user.cn())
        out_file.close()

# Makes an LDIF to change a pw
class changepwLDIF:
    def __init__(self, ldap_user):
        out_file = open(ldap_user.ldif(), 'w')
        out_file.write("dn: cn=%s,ou=Person,dc=fysik,dc=dtu,dc=dk\n" % ldap_user.cn())
        out_file.write("changetype: modify")
        out_file.write("replace: userpassword")
        out_file.write("userpassword: %s" % ldap_user.password())

