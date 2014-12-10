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
import urllib
from os import remove
from random import randrange
from subprocess import Popen,PIPE
from rn import split
# django imports
from django.core.mail import send_mail
from django.db.models.manager import EmptyManager
from django.db import models
from django.utils import timezone
from django.utils.encoding import smart_str
from django.contrib.auth.models import AbstractUser,UserManger
from django.contrib.auth.signals import user_logged_in
from django.contribn.contenttypes.models import ContentType
# project imports
from management.LDAP.LDAPComm import *
from management.LDAP.LDAPData import *
from management.LDAP.LDAPldiffer import changepwLDIF

#======================#
# mcUser login Updater #
#======================#
def update_last_login(sender, user, **kwargs):
    """
    A signal receiver which updates the last_login date for
    the user logging in.
    """
    mcUser.last_login = timezone.now()
    mcUser.save()
user_logged_in.connect(update_last_login)
#==========================#
# END mcUser login Updater #
#==========================#

#----------------------------------------------------------------------------------------------#

#=====================#
# mcUserManager CLASS #
#=====================#
class mcUserManager(models.Manager):
    self.userMan = models.OneToOneField(UserManger)

    def createMcUser(self, full_name, email=None, password): # PASSWORD IS NECESSARY
        now = timezone.now()
        if not full_name:
            raise ValueError('Please give Users Name')
        email = UserMan.normailise_email(email)
    
        mcuser = self.model()

        mcuser.save(using=self._db) # try to save the mcUser in the mcUserDB
#=========================#
# END mcUserManager CLASS #
#=========================#

#----------------------------------------------------------------------------------------------#

#==============#
# mcUser CLASS #
#==============#
class mcUser(models.Model):
    ident = None
    password = # TAKEN FROM LOGIN POST FORM, OR NONE
    authenticated = false
    user = models.OneToOneField(AbstractUser)
    ldap_user = LDAPData(user.username)       # HAVE TO BUILD THIS GUY ON INITIALISATION!
    conn = LDAPComm()

    UID         = models.CharField(_('uid'), max_length=5, unique=True, help_text=_('Unique id identifies user in LDAP and django sqlite DBs.') )
    displayName = models.CharField(_('Nickname'), max_length=10, unique=True, help_text=_('The name that is displayed during the session.') )
    email       = model.EmailField(_('e-mail address'), blank=True )
    is_staff    = models.BooleanField(_('Member of Staff'), default=False, help_text=('Allows admin access') )
    is_active   = models.BoolenaField(_('Enrolled on VNT Course'), default=True, help_text=('Currently enrolled on VNT course'
                                                                                            'Making this false instead of deleting keeps the user in the DB.') )
    last_login = models.DateTimeField(_('date joined'), default=timezone.now )

    objects = mcUserManager()

    if self.mcUser.get_full_name():
        self.conn.log("mcUser Opened Connection: %s" % self.mcUser.get_full_name())
        
    else:
        self.ident = "mcRnd::"+randRange(30)
        self.conn.log("New mcUser creation: %s" % self.ident)

    class Meta:
        verbose_name = _('mcUser')
        verbose_name_plural = _('mcUsers')

    def __unicode__(self):
        return self.displayName
    def natural_key(self):
        return self.UID
    def get_absolute_url(self):
        return "/mcUsers/%s/" % urllib.quote(smart_str(self.UID))
    def get_full_name(self):
        return self.ldap_user.cn()
    def getLDAPData(self):
        return ldap_user
    def is_anonymous(self):
        return False
    def is_authenticated(self, pw):
        return True
    '''
        return self.conn.authenticateMcUser(self.ldap_user.cn(), self.password) # MAYBE THIS, BUT FIND OUT WHERE IS CALLED SO PW CAN BE PASSED.
                                                                                # IF IT'S INTERNAL, LEAVE IT ALONE!!
    '''
    def has_usable_password():
        return self.checkPasswd()
    def get_group_permissions(self, obj=None):
        permissions = set()
        for backend in auth.get_backends():
            if hasattr(backend, "get_goup_poermissions"):
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
            if not self.has_perm()perm, obj):
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
                self.password = pw
                ''' RETURN TO BLANK POST PAGE '''
        except:
            self.conn.log("There was an exception when calling slappasswd: %s" % sys.exc_info()[0])
            # RETURN TO POST PAGE

    def check_password(self, pw):
        self.checkPasswd(pw)
    def checkPasswd(self, pw=self.password):
        self.conn.authenticateMcUser(self.ldap_user.cn(), pw)

    def authenticate(self):
        return mcBackend.authenticated(credentials[0].cn(), credentials[1])

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
#==================#
# END mcUser CLASS #
#==================#

#----------------------------------------------------------------------------------------------#


#
# mcBackend: Gets and Authenticates users from the LDAP DB.
#            Queries LDAP DB for uid and retrieves corresponding 
#            user from the sqlite DB.
#

class mcBackend(object):
    conn = LDAPComm()
    supports_interactive_user = True

    def get_user(uid):
        return mcUser.objects.get(uid=uid)

    def authenticate(cn, pw):
        uid = None
        if self.conn.authenticateMcUser(cn, pw):
            from management.LDAP.LDAPData import LDAPDataPopulator
            data = LDAPDataPopulator(cn, pw).getData()
            try:
                user = mcUser.objects.get(uid=data.uid())
                user.ldap_user = data
                return user 
            except User.DoesNotExist:
                return None
        return None
            

        

    


        


'''
class mcUserAdmin(models.Admin):
'''

