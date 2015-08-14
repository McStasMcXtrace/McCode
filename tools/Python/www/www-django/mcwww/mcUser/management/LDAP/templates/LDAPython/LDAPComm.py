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
#----------------#
# system imports #
#----------------#
import traceback
import sys
#----------------#
# python imports #
#----------------#
from re import split
from subprocess import call,check_output,Popen,PIPE
from cStringIO import StringIO

class LDAPComm:
#=======================================================================================#
# Builds an access log file and sets the query counter to zero.
#=======================================================================================#
    def __init__(self):
        from time import strftime as t
        file_loc = "mcUser/management/LDAP/LDAP_access/access.txt"
        self.access_file = open(file_loc, 'a+')
        self.access_file.write("\n\n\n\t\t\tNEW LOG\n\t\t\t-------\n\n\n")
        self.access_file.write("\n======================\n")
        self.access_file.write("LDAPComm instantiated : %s @ %s"% (t("%H:%M:%S"), t("%d/%m/%Y")))
        self.access_file.write("\n======================\n")        
        self.query_num = 0
#================#
# Access logging #
#================#
    def log(self, log_str):
        self.access_file.write("\n--------------------------------------------\n")
        self.access_file.write(log_str)
#=======================================================================================#
# Modification method: 
#=======================================================================================#
# General ldapAdd #
#=================#
    def ldapAdd(self, ldif_file, auth_dn, auth_pw):
        cn = split(",", auth_dn)[0]
        log_str = ""
        
        log_str += "auth_dn: %s, auth_pw: %s, ldif_file: %s\n"%(auth_dn, auth_pw, ldif_file)
        
        try:
            fid = Popen(["ldapadd", "-x", "-D", auth_dn, "-f", ldif_file, "-w", auth_pw],
                        stdout = PIPE,
                        stderr = PIPE)
            stdout, stderr = fid.communicate()
            log_str += "%s ADD attempt using : %s\n stdout:\n%s stderr:\n%s" % (cn, ldif_file, stdout, stderr)
            self.log(log_str)
        except:
            log_str += " Error adding %s:\n%s" % (ldif_file,stderr)
            log_str += " Backtrace:\n%s" % traceback.format_exc()   #sys.exc_info()
            self.log(log_str)
#=================#
# General ldapMod #
#=================#
    def ldapMod(self, ldif_file, auth_dn, auth_pw):
        cn = split(",", auth_dn)[0]
        log_str =""
        
        log_str += "auth_dn: %s, auth_pw: %s, ldif_file: %s\n"%(auth_dn, auth_pw, ldif_file)
        
        log_str += "%s MODIFICATION with: ldapmodify -x -D %s -f %s -w PASSWORD\n" % (cn, auth_dn, ldif_file)
        try:
            fid = Popen(["ldapmodify", "-x", "-D", auth_dn, "-f", ldif_file, "-w", auth_pw], #, "-v", "-d5"],
                        stdout=PIPE,
                        stderr=PIPE)
            stdout,stderr = fid.communicate()
            if 'modifying' in stdout:
                log_str += "%s MODIFIED: ldapmodify -x -D %s -f %s -w PASSWORD\n" % (cn, auth_dn, ldif_file)
                log_str += " stdout: \n%s\n"%(stdout)
                log_str += " stderr: \n%s\n"%(stderr)
            else:
                log_str += " Error modifying %s: \n stderr:\n%s" % (cn,stderr)
                print " Error:\n%s \n Backtrace:\n%s" % (stderr, traceback.format_exc())
            self.log(log_str)
        except:
            log_str += " ldapMod Error:\n %s\n" % str(sys.exc_info())
            self.log(log_str)
#=================#
# Verbose ldapMod #
#=================#
    def ldapModV(self, ldif_file, auth_dn, auth_pw):
        log_str = ""
        
        log_str += "auth_dn: %s, auth_pw: %s, ldif_file: %s\n"%(auth_dn, auth_pw, ldif_file)
        
        cn = split(",", auth_dn)[0]
        log_str = "%s MODIFY attempt with: %s\n" % (cn, ldif_file)
        try:
            fid = Popen(["ldapmodify", "-x", "-D", auth_dn, "-f", ldif_file, "-w", auth_pw, "-v", "-d5"],
                        stdout=PIPE,
                        stderr=PIPE)
            stdout,stderr = fid.communicate()
            if 'modifying' in stdout:
                log_str += "%s MODIFIED: ldapadd -x -D %s -f %s -w PASSWORD\n" % (cn, auth_dn, ldif_file)
                self.log(log_str) 
            else:
                log_str += " Error modifying %s:\n%s" % (cn,stderr)
                print "Error:%s \n Backtrace:%s" % (stderr, traceback.format_exc())
            self.log(log_str)
        except:
            log_str += "ldapMod Error: %s\n" % sys.exc_info()[0]
            self.log(log_str)
#=======================================================================================#
# This is a bit of a powerful thing to put in the build script but it should be ok. 
# As it's going to go in the build script then we should actually remove the sudo later
#=======================================================================================#
    def ldapSYSROOTmod(self, ldif_file):
        log_str = ""

        log_str += "EXTERNAL AUTHENTICATION, ldif_file: %s\n"%(ldif_file)
        
        log_str += "sudo ldapmodify -Y EXTERNAL -H ldapi:/// -f %s\n"% ldif_file
        try:
            log_str += ("MODIFICATION attempt of ROOTPWD: %s\n" % ldif_file)
            fid = Popen(["sudo", "ldapmodify", "-Y", "EXTERNAL", "-H", "ldapi:///", "-f", ldif_file], # fid = Popen(["sudo", "ldapmodify", "-Y", "EXTERNAL", "-H", "ldapi:///", "-f", ldif_file, "-v", "-d5"],
                        stdout=PIPE,
                        stderr=PIPE)
            stdout,stderr = fid.communicate()
            log_str += " stdout:\n%s stderr:\n%s" % (stdout,stderr)
        except:
            log_str += "Error creating LDAP ROOT user pwd: %s \n" % str(sys.exc_info())
        self.log(log_str)
#=======================================================================================#
# Query Methods
#=======================================================================================#
# General Query #
#===============#
    def ldapQuery(self, auth_dn, auth_pw, query):
        log_str = ""

        log_str += "auth_dn: %s, auth_pw: %s, ldif_file: %s\n"%(auth_dn, auth_pw, ldif_file)
        
        cn = split(",", auth_dn)[0]
        self.query_num += 1
        log_str += cn+" #"+str(self.query_num)+" QUERY with: ldapsearch -LLL -b D_N -D "+auth_dn+" -w PASSWORD "+query+"\n"
        try:
            fid = Popen(["ldapsearch", "-LLL", "-b", "D_N", "-D", auth_dn, "-w", auth_pw, query],
                        stdout=PIPE,
                        stderr=PIPE)
            stdout,stderr = fid.communicate()
            log_str += " stdout: %s\n stderr: %s\n"%(stdout, stderr)
        except:
            log_str += " Error:\n"
            for err_item in sys.exc_info():
                log_str += (err_item)
            self.log(log_str)
            pass
        self.log(log_str)
        return stdout.split("\n")
#==========================#
# Check existence in group #
#==========================#
    def ldapAdminGroupQuery(self, auth_cn):
        log_str = ""

        log_str += "auth_dn: %s, auth_pw: %s, ldif_file: %s\n"%(auth_dn, auth_pw, ldif_file)

        self.query_num += 1
        if not 'cn=' in auth_cn:
            auth_cn = "cn=%s" % auth_cn
        query = "(|(cn=itStaff)(cn=courseStaff))"
        log_str += "\n%s #%s AUTH ACCESS QUERY: %s\n" % (auth_cn, str(self.query_num), query)
        try:
            fid = Popen(
                ["ldapsearch", "-LLL", "-b", "D_N", "-D", "cn=DummyUser,ou=person,D_N", "-w", "DummyPW", query],
                stdout=PIPE,
                stderr=PIPE)
            stdout,stderr = fid.communicate()
            if cn in stdout:
                self.log(log_str)
                return True
            else:
                log_str += "LDAP privs insufficient: %s" % stderr
                self.log(log_str)
                return False
        except:
            log_str += "Incorrect search profile: %s => %s" % (query, sys.exc_info()[0])
            self.log(log_str)
            return False
#=====================#
# User Identification #
#=====================#
    def ldapAuthenticate(self, auth_cn, auth_pw):
        dn = "cn=\'" + auth_cn + "\',ou=person,D_N"
        log_str = ""
        f = Popen(["ldapwhoami", "-vvv", "-D", dn, "-x", "-w", auth_pw],
                  stdout=PIPE,
                  stderr=PIPE)
        print "auth_cn: ", auth_cn
        print "auth_pw: ", auth_pw
        print "dn: ", dn

        stdout,stderr = f.communicate()
        print "stdout: ",stdout
        print "stderr: ", stderr

        
        try:
            dn = "cn=\"" + auth_cn + "\",ou=person,D_N"
            try:
                fid = Popen(["ldapwhoami", "-vvv", "-D", dn, "-x", "-w", auth_pw],
                            stdout=PIPE,
                            stderr=PIPE)
                stdout,stderr = fid.communicate()
                print "stdout: ", stdout
                if "Success" in stdout:
                    log_str += "%s may access LDAP.\n"%dn
                    self.log(log_str)
                    return True
                else:
                    log_str += "Access attempt by %s unsuccessful: %s\n" % (dn, stderr)
                    self.log(log_str)
                    return False
            except:
                log_str += "Access attempt by %s exception: %s\n" % (dn, str(sys.exc_info()))
                self.log(log_str)
                ''' MESSAGE BOX SUGGESTING ERROR '''
                return False
        except:
            log_str += " Error: cn not supplied.\n"
            self.log(log_str)
            return False
