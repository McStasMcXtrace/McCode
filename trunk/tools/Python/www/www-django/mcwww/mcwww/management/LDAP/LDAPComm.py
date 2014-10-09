#=================================================#
# LDAP DB Modification and Authentication Methods #
# ----------------------------------------------- #
# Author: Mark Lewis                              #
#=================================================#
from subprocess import call,check_output,Popen,PIPE
from re import split as spl
from cStringIO import StringIO
import sys
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
# authenticateMcUser:
#   Takes auth_cn and auth_pw as argument, checks the LDAP DB ldapwhoami to check existence
#   returns boolean based on existence of user.
# ================================================================================================ #
class LDAPComm:
#=======================================================================================#
# Builds an access log file and sets the query counter to zero.
#=======================================================================================#
    def __init__(self):
        self.access_file = open("temp/accesses.txt", 'a+')
        self.query_num = 0
#================#
# Access logging #
#================#
    def log(self, log_str):
        self.access_file.write(log_str)
#=======================================================================================#
# Modification method: 
#=======================================================================================#
# General ldapAdd #
#=================#
    def ldapAdd(self, ldif_file, auth_dn, auth_pw):
        print "ldapAdd op"
        cn = spl(",", auth_dn)[0]
        log_str = cn+" ADDED ENTRY : ldapadd -x -D "+auth_dn+" -f %s -w PASSWORD"+ldif_file
        self.log(log_str)
        try:
            check_output(["ldapadd", "-x", "-D", auth_dn, "-f", ldif_file, "-w", auth_pw])
        except:
            log_str = "Error: " + str(sys.exc_info()[0])
            self.log(log_str)
#=================#
# General ldapMod #
#=================#
    def ldapMod(self, ldif_file, auth_dn, auth_pw):
        cn = spl(",", auth_dn)[0]
        log_str = cn+" MODIFICATION with: ldapmodify -x -D "+auth_dn+" -f "+ldif_file+" -w PASSWORD"
        self.log(log_str)
        try:
            check_output(["ldapmodify", "-x", "-D", auth_dn, "-f", ldif_file, "-w", auth_pw])
        except:
            log_str = "ldapMod Error: "+ str(sys.exc_info()[0])
            self.log(log_str)
#=================#
# Verbose ldapMod #
#=================#
    def ldapModV(self, ldif_file, auth_dn, auth_pw):
        cn = spl(",", auth_dn)[0]
        log_str = cn +" MODIFICATION with: ldapmodify -x -D "+ auth_dn +" -f "+ ldif_file +" -v -w PASSWORD"
        self.log(log_str)
        try:
            call(["ldapmodify", "-x", "-D", auth_dn, "-f", ldif_file, "-v", "-w", auth_pw]) 
        except:
            log_str = "Error: "+ str(sys.exc_info()[0])
            self.log(log_str)
#=======================================================================================#
# This is a bit of a powerful thing to put in the build script but it should be ok. 
# As it's going to go in the build script then we should actually remove the sudo later
#=======================================================================================#
    def ldapSYSROOTmod(self, ldif_file):
        log_str = "sudo ldapadd -Y EXTERNAL -H ldapi:/// -f config_pw_slapadd.ldif"
        self.log(log_str)
        try:
            call(["sudo", "ldapadd", "-Y", "EXTERNAL", "-H", "ldapi:///", "-f", ldif_file, "-v"])
        except:
            log_str ="Error: "+ str(sys.exc_info()[0]) 
            self.log(log_str)
#=======================================================================================#
# Query Methods
#=======================================================================================#
# General Query #
#===============#
    def ldapQuery(self, auth_dn, auth_pw, query):
        cn = spl(",", auth_dn)[0]
        ret_val = None
        pipe = PIPE
        self.query_num += 1
        log_str = cn + " QUERY with: ldapsearch -LLL -b dc=fysik,dc=dtu,dc=dk -D" + auth_dn + "-w PASSWORD " + query
        self.log(log_str)
        try:
            fid  = Popen(["ldapsearch", "-LLL", "-b", "dc=fysik,dc=dtu,dc=dk", "-D", auth_dn, "-w", auth_pw, query],
                         stdout=pipe,
                         stderr=pipe)
            stdout,stderr = fid.communicate()
            ret_val = stdout
        except:
            self.log("Error:")
            for err_item in sys.exc_info():
                self.log(err_item)
            pass
        return spl("\n", ret_val)
#==========================#
# Check existence in group #
#==========================#
    def ldapAdminGroupQuery(self, auth_cn):
        cn = "cn=%s" % auth_cn
        query = "(|(cn=itStaff)(cn=courseStaff))"
        log_str = cn +" AUTHORITY ACCESS QUERY with: ldapsearch -LLL -b ou=groups,dc=fysik,dc=dtu,dc=dk -D cn=DummyUser,ou=person,dc=fysik,dc=dtu,dc=dk -w DummyPW "+ query
        self.log(log_str)
        pipe = PIPE
        try:
            fid = Popen(
                ["ldapsearch", "-LLL", "-b", "dc=fysik,dc=dtu,dc=dk", "-D", "cn=DummyUser,ou=person,dc=fysik,dc=dtu,dc=dk", "-w", "DummyPW", query],
                stdout=pipe,
                stderr=pipe)
            stdout,stderr = fid.communicate()
            bill = stdout
            ben = stderr
            if cn in bill:
                return True
            else:
                log_str = "LDAP privs insufficient: "+ ben
                self.log(log_str)
                return False
        except:
            log_str = "Incorrect search profile: "+ query +" => "+ str(sys.exc_info()[0]) 
            self.log(log_str)
            return False
#=====================#
# User Identification # : in progress
#=====================#
    def ldapAuthenticate(self, auth_cn, auth_pw):
        dn = "cn=%s" + auth_cn + "ou=person,dc=fysik,dc=dtu,dc=dk"
        try:
            fid = Popen(["ldapwhoami", "-vvv", "-D", dn, "-x", "-w", auth_pw],
                        stdout=pipe,
                        stderr=pipe)
            stout,stderr = fid.communicate()
            bill = stdout
            ben = stderr
            if "Success" in bill:
                return True
            else:
                log_str = "Access attempt by "+ dn +" failed: "+ ben 
                self.log(log_str)
                return False
        except:
            log_str = "Access attempt by" + dn + "failed: " + str(sys.exc_info()[0])
            self.log(log_str)
            ''' MESSAGE BOX SUGGESTING ERROR '''
            return False
