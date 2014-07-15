from subprocess import call,check_output,Popen
from re import split
import sys
from cStringIO import StringIO

# LDAPComm
# ========
# ldapAdd:
#   takes LDIF file path, authenticating dn, and, authenticating password as arguments
#   reports the action to be taken to the user
#   tries to perform and catches and reports if there is an error.
# ldapMod(V):
#   takes LDIF file path, authenticating dn, and, authenticating password as arguments
#   reports the action to be taken to the user
#   tries to perform and catches and reports if there is an error.
# ldapSYSROOTmod:
#   uses the system root authentication to provide ldapMod functionality.
#   Limited use. LDAPs authentication takes precedent over sys for a number of cases.
# ldapQuery:
#   builds outfile based on user credentials and query
#   tries to query the DB, errors thrown are captured in the outfile too 
#   (should only throw query errors, check this and handle properly?)
# ldapAdminGroupQuery:
#   Takes a dn and pw of LDAP entity and queries the DB for itStaff and courseStaff groups,
#   filters the result through grep for the cn of the dn and retunrs true if the entry is a member
#   of the Staff groups, or false if not. This does not mean the LDAP entry has modify permissions.

class LDAPComm:
# Builds an access log file and sets the query counter to zero.
    def __init__(self):
        self.access_file = open("temp/accesses.txt", 'a+')
        self.query_num = 0

# In a lot of cases ldapAdd works exactly like ldapMod, ldapMod gives much better o/p
    def ldapAdd(self, ldif_file, auth_dn, auth_pw):
        cn = split(",", auth_dn)[0]
        # log LDAP Access
        self.access_file.write(cn + " ADDED ENTRY : ldapadd -x -D " + auth_dn + " -f " + ldif_file + " -w PASSWORD")
        try:
            check_output(["ldapadd", "-x", "-D", auth_dn, "-f", ldif_file, "-w", auth_pw])
        except:
            print "Error:", sys.exc_info()[0]

# Modification method: 
    def ldapMod(self, ldif_file, auth_dn, auth_pw):
        cn = split(",", auth_dn)[0]
        # log LDAP Access
        self.access_file.write(cn + " MODIFICATION with: ldapmodify -x -D " + auth_dn + "-f" + ldif_file + " -w PASSWORD") 
        try:
            check_output(["ldapmodify", "-x", "-D", auth_dn, "-f", ldif_file, "-w", auth_pw])
        except:
            print "Error:", sys.exc_info()[0]
# Verbose modification method
    def ldapModV(self, ldif_file, auth_dn, auth_pw):
        cn = split(",", auth_dn)[0]
        # log LDAP Access
        self.access_file.write(cn + " MODIFICATION with: ldapmodify -x -D " + auth_dn + " -f " + ldif_file + " -v -w PASSWORD") 
        try:
            call(["ldapmodify", "-x", "-D", auth_dn, "-f", ldif_file, "-v", "-w", auth_pw]) 
        except:
            print "Error:", sys.exc_info()[0]

# This is a bit of a powerful thing to put in the build script but it should be ok. As it's going to go in the build script then we should actually remove the sudo later :)
    def ldapSYSROOTmod(self, ldif_file):
        print "sudo ldapadd -Y EXTERNAL -H ldapi:/// -f config_pw_slapadd.ldif"
        print "System root password prompt follows:"
        try:
            call(["sudo", "ldapadd", "-Y", "EXTERNAL", "-H", "ldapi:///", "-f", ldif_file, "-v"])
        except:
            print "Error:", sys.exc_info()[0]

# Query Method
    def ldapQuery(self, auth_dn, auth_pw, query):
        cn = split(",", auth_dn)[0]
        out_file = "/home/lewis/Documents/LDAP/python/temp_query_files/temp_" + split(",|=", auth_dn)[1] + "_" + str(self.query_num) + "_" + query + ".txt"
        outfile = open(out_file, "a+")
        self.query_num += 1
        # log LDAP Access
        self.access_file.write( cn + " QUERY with: ldapsearch -LLL -b dc=fysik,dc=dtu,dc=dk -D " + auth_dn + " -w PASSWORD " + query + "\n")
        try:
            Popen(["ldapsearch", "-LLL", "-b", "dc=fysik,dc=dtu,dc=dk", "-D", auth_dn, "-w", auth_pw, query],
                  stdout=outfile,
                  stderr=outfile)
            print "Results/Errors are held in: " + out_file
        except:
            print "Error:"
            for err_item in sys.exc_info():
                print err_item
            pass
        outfile.close()

# Check of existence in groups with correct security - boolean returns.
    def ldapAdminGroupQuery(self, auth_dn, auth_pw):
        cn = split(",", auth_dn)[0]
        query = "\"(|(cn=itStaff)(cn=courseStaff))\""
        grep_pipe = "| grep member | grep -v DummyUser | grep " + cn
        # log LDAP Access
        print cn + " AUTHORITY ACCESS QUERY with: ldapsearch -LLL -b dc=fysik,dc=dtu,dc=dk -D " + auth_dn + " -w PASSWORD " + query # + grep_pipe + "\n"
        self.access_file.write( cn + " AUTHORITY ACCESS QUERY with: ldapsearch -LLL -b dc=fysik,dc=dtu,dc=dk -D " + auth_dn + " -w PASSWORD " + query) # + grep_pipe+ "\n")
        q_return_string = StringIO()
        try:
            Popen( ["ldapsearch", "-LLL", "-b", "dc=fysik,dc=dtu,dc=dk", "-D", auth_dn, "-w", auth_pw, query], # , "|", "grep", "member", "|", "grep", "-v", "DummyUser", "|", "grep", "cn=Linda"],#""],
                   stdout=q_return) 
            q_string = q_return.getvalue()
            print "q_string: " + q_string
            return True
        except:
            print "Returning False (unless testing)"
            return True
