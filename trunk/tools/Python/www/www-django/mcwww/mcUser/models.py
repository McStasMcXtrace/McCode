# ==================================#
# mcUser model, Backend and Manager #
# ------------                      #
# Provides authentication through   #
# LDAP DB                           #
# ------------------                #
# Author: Mark Lewis                #
# ==================================#

#================#
# system imports #
#================#
import sys
import urllib
import time
from datetime import date
from os import remove
#=================#
# general imports #
#=================#
from random import randrange
from subprocess import Popen,PIPE
from re import split
#================#
# django imports #
#================#
# from django.core.mail import send_mail - could implement so I get an email everytime the DB is accessed.
from django.db.models.manager import EmptyManager
from django.db import models
from django.utils import timezone
#====================#
# User Model imports #
#====================#
from django.contrib.auth.signals import user_logged_in
from django.contrib.contenttypes.models import ContentType
from django.contrib.auth.models import Group, AbstractBaseUser, BaseUserManager
#==============#
# LDAP imports #
#==============#
from mcUser.management.LDAP import LDAPComm
from mcUser.management.LDAP.LDAPData import *
from mcUser.management.LDAP.LDAPldiffer import changepwLDIF

#----------------------------------------------------------------------------------------------#

#======================#
# mcUser login Updater #
#======================#
def update_last_login(sender, user, **kwargs):
    """
    receiver: updates last_login for user logging in.
    """
    user.last_login = timezone.now()
    user.save()
user_logged_in.connect(update_last_login) # called as soon as this file is included.
#==========================#
# END mcUser login Updater #
#==========================#

#----------------------------------------------------------------------------------------------#

#=================#
# mcBackend CLASS # -  mcBackend: Gets and Authenticates users from the LDAP DB. 
#=================#               Queries LDAP DB for uid and retrieves corresponding user from the sqlite DB. 
class mcBackend(object):
    conn = LDAPComm.LDAPComm()
    supports_interactive_user = True
    def get_user(self, uid):
        # Need to put the LDAP details in the mcUser model at this stage
        try:
            return mcUser.objects.get(UID=uid) # have to build the django DB first
        except UserModel.DoesNotExist:
            return None

    def authenticate(self, uid, pw):
        in_log = open("in.log", 'a')
        t = time.time()
        in_log.write("\nTimestamp: %s:%s\n" % (str(date.fromtimestamp(t)), str(t)) )
        in_log.write("Finding data from uid: "+ uid +"\n")
        self.data = LDAPData()

        def get_cn(UID):
            import base64
            print "in models:authenticate, UID=", UID
            dn = None
            ldap_data = self.conn.ldapQuery('cn=DummyUser,ou=person,dc=branch',    # TEMPLATE LINE
                                            'DPW',                                             # TEMPLATE LINE
                                            "uid=%s"%UID)            
            print "ldap_data:\n",ldap_data
            for line in ldap_data:
                print "evaluating %s"%line
                if 'uid::' in line:
                    self.data.setuid(base64.standard_b64decode(split(" ", line)[1]).strip())
                if 'cn::' in line:
                    self.data.setcn(base64.standard_b64decode(split(" ", line)[1]).strip())
                if 'sn::' in line:
                    self.data.setsn(base64.standard_b64decode(split(" ", line)[1]).strip())
                if 'mail::' in line:
                    self.data.setmail(base64.standard_b64decode(split(" ", line)[1]).strip())
                if 'displayname::' in line:
                    self.data.setdisplayname(base64.standard_b64decode(split(" ", line)[1]).strip())
                if line: in_log.write("%s\n"%line)
                if "dn:" in line:
                    in_log.write("Found data: %s \n"% split(",|=", line)[1])
                    dn = split(",|=", line)[1].strip()
            if dn: 
                in_log.close()
                return dn
            in_log.write("Usr not found from uid: %s\n"%UID)
            in_log.close()
            return None

        cn = get_cn(uid)
        print "uid: ", uid
        print "cn: ", cn
        if self.conn.ldapAuthenticate(cn, pw):
            try:
                user = mcUser.objects.get(uid=self.data.uid())
                user.ldap_user = self.data
                update_last_login(self, user)
                return user 
            except User.DoesNotExist:
                return None
        return None

    def session_login(self, user):
        print "in here we should log the mcUser into the mcSession."
#=====================#
# END mcBackend CLASS #
#=====================#

#----------------------------------------------------------------------------------------------#

#=====================#
# mcUserManager CLASS #
#=====================#
class mcUserManager(BaseUserManager): 
    def createMcUser(self, usr_details):
        mcuser = self.model()
        mcuser.uid         = usr_details['uid']
        mcuser.username    = usr_details['username']
        mcuser.displayName = usr_details['displayname']
        mcuser.email       = usr_details['email']
        mcuser.is_staff    = usr_details['staff']
        mcuser.is_active   = True
        mcuser.last_login  = timezone.now()
        mcuser.save(using=self._db) 
        return mcuser
    
    def create_superuser(self, username, email, password, **extra_fields):
        for k,v in usr_details.items():
            print k, ":", v
        return self._create_user(username, email, password, True, True,
                                 **extra_fields)

        
#=========================#
# END mcUserManager CLASS #
#=========================#

#----------------------------------------------------------------------------------------------#

#==============#
# mcUser CLASS #
#==============#
class mcUser(AbstractBaseUser):
    def __init__(self, *args, **kwargs):
        super(mcUser, self).__init__(*args, **kwargs)
        #-------------------------#
        # mcUser class attributes #
        #-------------------------#
        self.authenticated = False
        self.ldap_user = LDAPData() # populated on authentication or empty.
        self.conn = LDAPComm.LDAPComm() 
    #---------------------#
    # mcUser model fields #
    #---------------------#
    uid            = models.CharField(('uid'), max_length=5, unique=True, help_text=('Unique id identifies user in LDAP and django sqlite DBs.'))#, primary_key=True)
    USERNAME_FIELD =  'uid'
    username       = models.CharField(('username'), max_length=30, unique=False, help_text=('Non-unique id identifies user in django DB, used to create unique LDAP/django sqlite ID'))
    is_staff       = models.BooleanField(('Member of Staff'), default=False, help_text=('Allows admin access') )
    is_active      = models.BooleanField(('Enrolled on VNT Course'), default=True, help_text=('Currently enrolled on VNT course.'
                                                                                              'Making this false instead of deleting keeps the user in the DB',
                                                                                              'but stops them accessing the course.'))
    displayName     = models.CharField(('Nickname'), max_length=10, unique=True, help_text=('The name that is displayed during the session.') )
    email           = models.EmailField(('e-mail address'))
    REQUIRED_FIELDS = ['is_staff', 'is_active', 'last_login', 'displayName', 'email']

    objects = mcUserManager()
    class Meta:
        verbose_name = ('mcUser')
        verbose_name_plural = ('mcUsers')

    def __unicode__(self):
        return self.displayName
    def natural_key(self):
        return self.uid
    def get_absolute_url(self):
        return "/mcUsers/%s/" % urllib.quote(smart_str(self.uid))
    def get_full_name(self):
        return self.ldap_user.cn()
    def getLDAPData(self):
        return self.ldap_user
    def is_anonymous(self):
        return False
    def is_authenticated(self, pw):
        return self.authenticated
    '''
        return self.conn.authenticateMcUser(self.ldap_user.cn(), self.password) # MAYBE THIS, BUT FIND OUT WHERE IS CALLED SO PW CAN BE PASSED.
                                                                                # IF IT'S INTERNAL, LEAVE IT ALONE!!
    '''
    def has_usable_password(self):
        return self.checkPasswd()
    def get_group_permissions(self, obj=None):
        permissions = set()
        for backend in auth.get_backends():
            if hasattr(backend, "get_group_permissions"):
                if obj is not None:
                    permissions.update(backend.get_group_permissions(self, obj))
                else:
                    permissions.update(backend.get_group_permissions(self))
        return permissions
    '''
    def get_all_permissions(self, obj=None):
        return _user_get_all_permissions()
    '''
    def has_perms(self):
        for perm in perm_list:
            if not self.has_perm(perm, obj):
                return False
        return True

    def set_password(self, pw):
        self.setPasswd(pw)
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
                elif self.ident:
                    self.conn.log("LDAP user not set up: %s" % self.ident)
                    ''' RETURN TO POST PAGE '''
                else:
                    self.ldap_user.setldif_file("temp/%spw.ldif" % self.ldap_user.cn())
                    self.ldap_user.setpassword(pwd)
                    changepwLDIF(self.ldap_user)
                    self.conn.ldapMod(self.ldap_user.ldif(), '''POST_FORM_USER_dc=branch''', '''POST_FORM_USER_PW''')
                    self.conn.log("uid:%s password changed." % uid)                
                    remove(self.ldap_user.ldif())
                    self.ldap_user.setldif_file(None)
                    self.ldap_user.setpassword(None)
                    ident = None
                    self.password = pw
                    ''' RETURN TO BLANK POST PAGE '''
            except:
                self.conn.log("There was an exception when calling slappasswd: %s" % sys.exc_info()[0])
                                      # RETURN TO POST PAGE
                
    def check_password(self, pw):
        self.conn.authenticateMcUser(self.ldap_user.cn(), pw)

    def authenticate(self):
        self.authenticated = mcBackend.authenticate(self.ldap_user.cn(), pwd)
        return self.authenticated

    def get_profile(self):
        if not hasattr(self, '_profile_cache'):
            from django.conf import settings
            if not getattr(settings, 'AUTH_PROFILE_MODULE', False):
                raise SiteProfileNotAvailable(
                    'You need to set AUTH_PROFILE_MODULE in your project '
                    'settings')
            try:
                app_label, model_name = settings.AUTH_PROFILE_MODULE.split('.')
            except ValueError:
                raise SiteProfileNotAvailable(
                    'app_label and model_name should be separated by a dot in '
                    'the AUTH_PROFILE_MODULE setting')
            try:
                model = models.get_model(app_label, model_name)
                if model is None:
                    raise SiteProfileNotAvailable(
                        'Unable to load the profile model, check '
                        'AUTH_PROFILE_MODULE in your project settings')
                self._profile_cache = model._default_manager.using(
                                   self._state.db).get(user__id__exact=self.id)
                self._profile_cache.user = self
            except (ImportError, ImproperlyConfigured):
                raise SiteProfileNotAvailable
        return self._profile_cache

    def __str__(self):
        return str(self.id)+":"+self.uid+":"+self.username
    # instatiation handling
#    if get_full_name():
#       self.conn.log("mcUser Opened Connection: %s" % get_full_name())
#    else:
#        self.ident = "mcRnd::"+randRange(30)
#        self.conn.log("New mcUser creation: %s" % self.ident)

#==================#
# END mcUser CLASS #
#==================#

#----------------------------------------------------------------------------------------------#


'''
class mcUserAdmin(models.Admin):
'''

