#=======================================================================================#
# LDAPComm                                                                              #
# --------                                                                              #
# ldapAdd:                                                                              #
#   takes LDIF file path, authenticating dn, and, authenticating password as arguments  #
#   reports the action to be taken to the user                                          #
#   tries to perform and catches and reports if there is an error.                      #
# ldapMod(V):                                                                           #
#   takes LDIF file path, authenticating dn, and, authenticating password as arguments  #
#   reports the action to be taken to the user                                          #
#   tries to perform and catches and reports if there is an error.                      #
# ldapSYSROOTmod:                                                                       #
#   uses the system root authentication to provide ldapMod functionality.               #
#   Limited use. LDAPs authentication takes precedent over sys for a number of cases.   #
# ldapQuery:                                                                            #
#   builds outfile based on user credentials and query                                  #
#   tries to query the DB, errors thrown are captured in the outfile too                #
#   (should only throw query errors, check this and handle properly?)                   #
# ldapAdminGroupQuery:                                                                  #
#   Takes a dn and pw of LDAP entity and queries the DB for itStaff and courseStaff     #
#   groups, filters the result through grep for the cn of the dn and retunrs true if    #
#   the entry is a member of Staff groups, or false if not. This does not mean the LDAP #
#   entry has modify permissions.                                                       #
# authenticateMcUser:                                                                   #
#   Takes auth_cn and auth_pw as argument, checks the LDAP DB ldapwhoami to check       #
#   existence returns boolean based on existence of user.                               #
# ------------------                                                                    #
# Author: Mark Lewis                                                                    #
#=======================================================================================#
from subprocess import call,check_output,Popen,PIPE
from re import split
import sys
from cStringIO import StringIO

class LDAPComm:
#=======================================================================================#
# Builds an access log file and sets the query counter to zero.
#=======================================================================================#
    def __init__(self):
        file_loc = "mcUser/management/LDAP/LDAP_access/access.txt"
        self.access_file = open(file_loc, 'a+')
        self.query_num = 0
#================#
# Access logging #
#================#
    def log(self, log_str):
        self.access_file.write(log_str+"\n")
#=======================================================================================#
# Modification method: 
#=======================================================================================#
# General ldapAdd #
#=================#
    def ldapAdd(self, ldif_file, auth_dn, auth_pw):
        cn = split(",", auth_dn)[0]
        self.log("%s ADDED ENTRY : ldapadd -x -D %s -f %s -w PASSWORD" % (cn, auth_dn, ldif_file))
        try:
            check_output(["ldapadd", "-x", "-D", auth_dn, "-f", ldif_file, "-w", auth_pw])
        except:
            self.log("Error: %s" % sys.exc_info()[0])
#=================#
# General ldapMod #
#=================#
    def ldapMod(self, ldif_file, auth_dn, auth_pw):
        cn = split(",", auth_dn)[0]
        self.log("%s MODIFICATION with: ldapmodify -x -D %s -f %s -w PASSWORD" % cn, auth_dn, ldif_file) 
        try:
            check_output(["ldapmodify", "-x", "-D", auth_dn, "-f", ldif_file, "-w", auth_pw])
        except:
            self.log("ldapMod Error: %s" % sys.exc_info()[0])
#=================#
# Verbose ldapMod #
#=================#
    def ldapModV(self, ldif_file, auth_dn, auth_pw):
        cn = split(",", auth_dn)[0]
        self.log("%s MODIFICATION with: ldapmodify -x -D %s -f %s -v -w PASSWORD" % (cn, auth_dn, ldif_file) )
        try:
            call(["ldapmodify", "-x", "-D", auth_dn, "-f", ldif_file, "-v", "-w", auth_pw]) 
        except:
            self.log("Error: %s" % sys.exc_info()[0])
#=======================================================================================#
# This is a bit of a powerful thing to put in the build script but it should be ok. 
# As it's going to go in the build script then we should actually remove the sudo later
#=======================================================================================#
    def ldapSYSROOTmod(self, ldif_file):
        self.log("sudo ldapadd -Y EXTERNAL -H ldapi:/// -f config_pw_slapadd.ldif")
        try:
            call(["sudo", "ldapadd", "-Y", "EXTERNAL", "-H", "ldapi:///", "-f", ldif_file, "-v"])
        except:
            self.log("Error: %s" % sys.exc_info()[0])
#=======================================================================================#
# Query Methods
#=======================================================================================#
# General Query #
#===============#
    def ldapQuery(self, auth_dn, auth_pw, query):
        cn = split(",", auth_dn)[0]
        ret_val = None
        pipe = PIPE
        self.query_num += 1
        log_str = cn + " QUERY with: ldapsearch -LLL -b DN -D" + auth_dn + "-w PASSWORD " + query+"\n"
        self.log(log_str)
        try:
            fid  = Popen(["ldapsearch", "-LLL", "-b", "DN", "-D", auth_dn, "-w", auth_pw, query],
                         stdout=pipe,
                         stderr=pipe)
            stdout,stderr = fid.communicate()
            ret_val = stdout
        except:
            self.log("Error:")
            for err_item in sys.exc_info():
                self.log(err_item)
            pass
        return ret_val.split("\n")
#==========================#
# Check existence in group #
#==========================#
    def ldapAdminGroupQuery(self, auth_cn):
        cn = "cn=%s" % auth_cn
        query = "(|(cn=itStaff)(cn=courseStaff))"
        self.log("%s AUTHORITY ACCESS QUERY with: ldapsearch -LLL -b ou=groups,DN -D cn=DummyUser,ou=person,DN -w DummyPW %s\n" % (cn, query) )
        try:
            fid = Popen(
                ["ldapsearch", "-LLL", "-b", "DN", "-D", "cn=DummyUser,ou=person,DN", "-w", "DummyPW", query],
                stdout=PIPE,
                stderr=PIPE)
            stdout,stderr = fid.communicate()
            bill = stdout
            ben = stderr
            if cn in bill:
                return True
            else:
                self.log("LDAP privs insufficient: %s" % ben)
                return False
        except:
            self.log("Incorrect search profile: %s => %s" % query, sys.exc_info()[0])
            return False
#=====================#
# User Identification #
#=====================#
    def ldapAuthenticate(self, auth_cn, auth_pw):
        dn = "cn=%s" + auth_cn + ",ou=person,DN"
        try:
            fid = Popen(["ldapwhoami", "-vvv", "-D", dn, "-x", "-w", auth_pw],
                        stdout=PIPE,
                        stderr=PIPE)
            stdout,stderr = fid.communicate()
            bill = stdout
            ben = stderr
            if "Success" in bill:
                return True
            else:
                self.log("Access attempt by %s failed: %s" % (dn, ben) )
                return False
        except:
            log_str = "Access attempt by" + dn + "failed: " + str(sys.exc_info()[0])
            self.log(log_str)
            ''' MESSAGE BOX SUGGESTING ERROR '''
            return False
