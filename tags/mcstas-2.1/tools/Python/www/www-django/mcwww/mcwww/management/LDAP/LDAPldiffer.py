#from mcwww.management.LDAP import LDAPData
import LDAPData
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

#============================#
# Add User LDIF file creator #
#============================#
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
        out_file.write("-")
        out_file.write("dn: cn=%s,ou=groups,dc=fysik,dc=dtu,dc=dk" % ldap_usr.group())
        out_file.write("changetype: modify")
        out_file.write("add: member")
        out_file.write("member: cn=%s,ou=person,dc=fysik,dc=dtu,dc=dk" % ldap_user.cn())
        out_file.close()
#===================================#
# Change Password LDIF file creator #
#===================================#
class changepwLDIF:
    def __init__(self, ldap_user):
        out_file = open(ldap_user.ldif(), 'w')
        out_file.write("dn: cn=%s,ou=Person,dc=fysik,dc=dtu,dc=dk\n" % ldap_user.cn())
        out_file.write("changetype: modify")
        out_file.write("replace: userpassword")
        out_file.write("userpassword: %s" % ldap_user.password())
#===========================================================#
# The builder should be modified to take input and populate #
# these ldifs before it modifies the LDAP DB                #
#===========================================================#
class initialSetupLDIFs:
    def __init__(self, db_dn, mail):
        self.file_path = "setupLDIFs/"
        self.adminEmail = mail
        self.db = db_dn
        
    def ROOTPW(self, pw):
        out = open(self.file_path + "ROOTPW.ldif")
        out.write("dn: olcDatabase={0}config,cn=config\n" ++
                  "changetype: modify\n" ++
                  "add: olcRootPW" ++
                  "olcRootPW: " ++ pw )
        out.close()

    def DIT(self, ou_branches, dummy_pw, bind_user, bind_pw):
        out = open(self.file_path + "DITbuild.ldif")
        out.write("# DIT branches")
        # This should be defaulted as: ["person","groups","access"]
        # but can be user defined.                        
        for ou in ou_branches:
            out.write("dn: "+self.db+"\n"+
                      "objectclass: organizationalunit"+
                      "ou: "+ou+"\n"+
                      "description: Branch containing "+ou+" entries.\n"
                      "\n")
        out.write("# DummyUser")
        out.write("dn: cn=DummyUser,ou="+ou_branches[0]+","+self.db+"\n"+
                  "objectclass: inetorgperson\n"+
                  "cn: DummyUser\n"+
                  "sn: Searcher\n"+
                  "uid: DU\n"+
                  "userpassword: "+dummy_pw+
                  "description: Person entity. Initialises groups, searches ldap uids.\n"+
                  "ou: person\n"+
                  "\n")
        out.write("# Moodle/MediaWiki bind user")
        out.write("dn: cn="+bind_user+","+ou_branches[2]+","+db+"\n"+
                  "objectclass: inetorgperson\n"+
                  "cn: "+bind_user+"\n"+
                  "sn: Searcher\n"+
                  "uid: AuthBindUser\n"+
                  "userpassword: "+bind_pw+
                  "description: Person entity. Provides LDAP authentication.\n"+
                  "ou: "+ou_branches[2]+"\n"+
                  "\n")
        out.write("# groups subtree")
        for group in ["courseStaff","itStaff","Student"]:
            out.write("dn: cn="+group+",ou="+ou_branches[1]+","+self.db+"\n"+
                      "objectclass: groupofnames\n"+
                      "cn: "+group+"\n"+
                      "member: cn=DummyUser,ou="+ou_branches[0]+","+self.db+"\n"
                      "\n")
    def ACCESSbuild(self):
        print "put the access control here"
    def INITPOPbuild(self):
        print "put the main admin user(s) here"
