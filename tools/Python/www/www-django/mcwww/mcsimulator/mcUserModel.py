# Mark Lewis - 1-8-14
# ===================
'''
#-----------------------------------------------------------#
| - models in this class build the mcUser model and mcAdmin |
|   model and associated models necessary to connect to the |
|   LDAP DB and authentication                              |
#-----------------------------------------------------------#
'''
# python imports
import sys
from os import remove
from random import randrange
from subprocess import Popen,PIPE
from rn import split
# django imports
from django.contrib.auth.models import AbstractUser,UserManger
# project imports
from management.LDAP.LDAPComm import *
from management.LDAP.LDAPData import *
from management.LDAP.LDAPldiffer import changepwLDIF


#
# mcUser - user      : extension of the user model by including a one-one
#                      reference/instance.                                     <--REFERENCES TO User MODEL CAN BE CHANGED TO mcUser.user
#        - ldap_user : LDAPData instance holding the LDAP information 
#                      on this particular user.
#        - manager   : instance of the managing object that allows 
#                      manipulation of the user.
#        - set_password()
#          ------------
#          Open a pipe to record io events from the system in stdout and stderr.
#          If there is an error quit.
#          If there is an exception (don't see why) then report that and quit.
#          Build a pw change LDIF, and run it.
#          Do not hold the pw.
#        - get_LDAPData() :  returns the LDAPData object that correspond to the user
#          ------------

class mcUser(models.Model):
    # Functional Objects and Variables #
    ident = None
    authenticated = false
    user = models.OneToOneField(AbstractUser)
    ldap_user = LDAPData(user.username)
    manager = mcUserManager()
    conn = LDAPComm()
    # Additional DB Fields #
    UID = models.CharField(_('uid'), max_length=5, unique=True,
                           help_text=_('Unique id identifies user in LDAP and django sqlite DBs')
                           )
    # Logging of 
    if self.user.get_full_name():
        self.conn.log("mcUser Opened Connection: %s" % self.user.get_full_name())
    else:
        self.ident = "mcRnd::"+randRange(30)
        self.conn.log("New mcUser creation: %s" % self.ident)

    # THIS NEEDS A FEW BITS FILLING IN AND MAYBE MOVING TO A MORE SUITABLE LOCATION #
    def setPasswd(self, pw):
        pipe = PIPE
        try:
            fid = Popen(["slappasswd", "-s", pw], stdout=pipe, stderr=pipe)
            stdout,stderr = fid.communicate()
            pwd = stdout
            err_msg = stderr
            if err_msg:
                self.conn.log("There was an error creating the password: %s" % err_msg)
                ''' RETURN TO POST PAGE '''
            elif ident:
                self.conn.log("LDAP user not set up: %s" % ident)
                ''' RETURN TO POST PAGE '''
            else:
                self.ldap_user.setldif_file("temp/%spw.ldif" % self.ldap_user.cn())
                self.ldap_user.setpassword(pwd)
                changepwLDIF(ldap_user)
                conn.ldapMod(self.ldap_user.ldif(), '''POST_FORM_USER_DN''', '''POST_FORM_USER_PW''')
                self.conn.log("%s password changed." % self.user.get_full_name())                
                remove(self.ldap_user.ldif())
                self.ldap_user.setldif_file(None)
                ldap_user.setpassword(None)
                ident = None
                ''' RETURN TO BLANK POST PAGE '''
        except:
            self.conn.log("There was an exception when calling slappasswd: %s" % sys.exc_info()[0])
            # RETURN TO POST PAGE

    def authenticated(self):
        return self.authenticated(credentials[0].cn(), credentials[1])

    def checkPasswd(self, pw):
        self.conn.authenticateMcUser(self.ldap_user.cn(), pw)

    def getLDAPData(self):
        return ldap_user

    def getUID(self):
        return ldap_user.uid()
    
# END mcUser CLASS




# THIS CLASS IS INCOMPLETE, NEED TO FIND HOW TO ACCESS LOGGED IN USER INFO.

class mcUserManager(models.Manager):
    self.userMan = models.OneToOneField(UserManger)
    def createMcUser(self, name, email=None, password): # PASSWORD IS NECESSARY
        LDAP_admin_cn = # GET CURRENT ADMIN USER SOMEHOW - THEY SHOULD BE IN THE LDAP DB AS ADMIN GROUP MEMBERS
        if(not comm.ldapAdminGroupQuery(LDAP_admin_cn)): 
            ''' 
            THERE ACTUALLY NEEDS TO BE A BIT OF PARSING HERE TO DETERMINE THE OP.
            PUT THIS IN A RESPONSE BOX LIKE THE SIMULATION SAVE ERRORS
            "Insufficient LDAP privs. Sorry :("
            '''
        entity = LDAPUserCreation(username, password)
        LDAP_admin_pw = # GET THE CURRENTLY LOGGED IN ADMINS PW
        LDAP_admin_dn = "cn=%s,ou=person,dc=fysik,dc=dtu,dc=dk" % # GET CURRENT LOGGED IN ADMINS CN
        entity.processLDIF(LDAP_admin_dn, LDAP_admin_pw)
        

#
# **credentials[0] = LDAPData object
# **credentials[1] = password
#

class mcBackend(object):
    conn = LDAPComm()
    def get_user(uid):
        return '''MCUSER OBJECT'''
    def authenticate(**credentials):
        if self.conn.authenticateMcUser():
            return ''' MCUSER OBJECT BASED ON credentials[0].uid()'''
        
        # parse the return query for access/authentication
        # request the specified django user based on uid return
        # return django user.
  
#
# full_name[0]      = first name
# full_name[1..n-1] = middle names
# full_name[N]      = last name
#

        


'''
class mcUserAdmin(models.Admin):
'''

