#==========================================#
# createsuperuser.py                       #
# ------------------                       #
# Management utility to create superusers. #
# Templated from django.auth commands      #
# ---------------                          #
# Edited by: Mark Lewis                    #
#==========================================#
from __future__ import unicode_literals
#----------------#
# python imports #
#----------------#
import getpass
import sys
from optparse import make_option
#----------------#
# django imports #
#----------------#
from django.contrib.auth import get_user_model
from django.contrib.auth.management import get_default_username
from django.core import exceptions
from django.core.management.base import BaseCommand, CommandError
from django.db import DEFAULT_DB_ALIAS
from django.utils.encoding import force_str
from django.utils.six.moves import input
from django.utils.text import capfirst
#-------------#
# app imports #
#-------------#
from mcUser.models import *
from creation_helpers import makeuid, usrCheck, checkLDAPperms

class NotRunningInTTYException(Exception):
    pass

#
# - self.UserModel -> mcUser EVERYTIME, this is non-negotiable. (plus makes life easier)
#   - get_user_model can go
#   - get_default_user_name : does this implementation have to change?
# - capfirst uneccessary : mcUser fields are what they are
# - make create mcUser complete class to be called by createuser and createsuperuser? <- this most probably
# !!! MOAR REFACTORING REQUIRED !!!


class Command(BaseCommand):
    def __init__(self, *args, **kwargs):
        # Options are defined in an __init__ method to support swapping out
        super(Command, self).__init__(*args, **kwargs)



        # this is always going to be mcUser
        #        self.UserModel = get_user_model()
        self.User = mcUser.new()
        # this is always going to be mcUser.uid
        self.username_field = self.UserModel._meta.get_field(self.UserModel.USERNAME_FIELD)

        print "User model is: "+str(self.UserModel)+"\n"



        self.option_list = BaseCommand.option_list + 
        (make_option('--%s' % self.UserModel.USERNAME_FIELD, # this is mcUser::uid
                     dest=self.UserModel.USERNAME_FIELD, 
                     default=None,
                     help='Specifies the login for the superuser.'
                     ),
         make_option('--noinput', 
                     action='store_false', 
                     dest='interactive', 
                     default=True,
                     help=('Tells Django to NOT prompt the user for input of any kind. '
                           'You must use --%s with --noinput, along with an option for '
                           'any other required field. Superusers created with --noinput will '
                           ' not be able to log in until they\'re given a valid password.' % self.UserModel.USERNAME_FIELD)
                     ),
         make_option('--database',                           # this is mcwww.sqlite, but std way to treat.
                     action='store', 
                     dest='database',
                     default=DEFAULT_DB_ALIAS, 
                     help='Specifies the database to use. Default is "default".'
                     ),
         ) + 
        tuple(make_option('--%s' % field,                    # this will be used because it's an easy way to
                          dest=field,                        # make a form from the admin site....don't hate me :(
                          default=None,
                          help='Specifies the %s for the superuser.' % field
                          ) for field in self.UserModel.REQUIRED_FIELDS
              )
        option_list = BaseCommand.option_list
        help = 'Used to create a superuser.'
        
    def execute(self, *args, **options):
        self.stdin = options.get('stdin', sys.stdin)
        return super(Command, self).execute(*args, **options)


#=====================================================#
# handle()                                            #
# --------                                            #
# REIMPLEMENT THIS FOR LDAP AND SuperMcUser CREATION. #
#=====================================================#
    def handle(self,  *args, **options):
        checkLDAPperms()
        usr_details = {}
        usr_details['staff'] = True
        usr_details['email'] = None
        usr_details['username'] = options.get(self.User.uid, None)
        #        usr_details['username'] = options.get(self.UserModel.USERNAME_FIELD, None)
        interactive = options.get('interactive')
        verbosity = int(options.get('verbosity', 1))
        database = options.get('database')
        # If not provided, create the user with an unusable password
        usr_details['password'] = None
        #--------------------------------------------#
        # Fairly pointless block for our use         #
        # Do quick and dirty validation if --noinput #
        # Should take the --noinput flag out?        #
        #--------------------------------------------#
        if not interactive:
            try:
                if not usr_details['username']:
                    raise CommandError("You must use --%s with --noinput." % self.UserModel.USERNAME_FIELD)
                usr_details['username'] = self.username_field.clean(usr_details['username'], None)      
                # Work out what this does -  but it will be interactive unless I popoulate it
                #                            with a form....
                for field_name in self.UserModel.REQUIRED_FIELDS:
                    if options.get(field_name):
                        field = self.UserModel._meta.get_field(field_name)
                        usr_details[field_name] = field.clean(options[field_name], None)
                    else:
                        raise CommandError("You must use --%s with --noinput." % field_name)
            except exceptions.ValidationError as e:
                raise CommandError('; '.join(e.messages))
            
        #---------------------#
        # Actual code we need #
        #---------------------#
        else:
            # now I have to find out how this works and if I can cirumvent it...
            default_username = get_default_username()
            try:
                if hasattr(self.stdin, 'isatty') and not self.stdin.isatty(): raise NotRunningInTTYException("Not running in a TTY")
                verbose_field_name = self.username_field.verbose_name
                #-------------------#
                # Username creation #
                #-------------------#
                while usr_details['username'] is None:
                    if not usr_details['username']:
                        input_msg = verbose_field_name
                        if default_username:
                            input_msg = "%s (leave blank to use '%s')" % (input_msg, default_username)
                        raw_value = input(force_str('%s: ' % input_msg))
                    if default_username and raw_value == '': raw_value = default_username
                    try:
                        usr_details['username'] = self.username_field.clean(raw_value, None)
                    except exceptions.ValidationError as e:
                        self.stderr.write("Error: %s" % '; '.join(e.messages))
                        usr_details['username'] = None
                        continue
                    # testing user existence in sqlite db - could employ this in createuser.py?
                    try:
                        # which of these?
                        # ---------------
                        # mcUser.objects. get_by_natural_key(... ?
                        # mcUser.objects. db_manager(database).get_by_natural_key(... ?
                        self.UserModel._default_manager.db_manager(database).get_by_natural_key(usr_details['username'])
                    except self.UserModel.DoesNotExist:
                        pass
                    else:
                        self.stderr.write("Error: That uid is already taken.") # figured a way round this in createuser
                        usr_details['username'] = None                         # DO THAT, IT'S NICER --------^
                usr_details['uid']   = makeuid(usr_details['username'])
                #-----------------------#
                # end Username creation #
                #-----------------------#

                #----------------#
                # email creation # seems to be the only one I need to get them to input
                #----------------#
                while user_data['email'] is None:
                    raw_value = input(force_str('email: '))
                    try:
                        user_data['email'] = field.clean(raw_value, None)
                    except exceptions.ValidationError as e:
                        self.stderr.write("Error: %s" % '; '.join(e.messages))
                        user_data['email'] = None
                #--------------------#
                # end email creation #
                #--------------------#

                #-------------------------------------#
                # Password creation                   #
                # -----------------                   #
                # Remember:                           #
                #     slappasswd                      #
                #     no passwd in mcUser sqlite DB!! #
                #-------------------------------------#
                while usr_details['password'] is None:
                    if not usr_details['password']:
                        usr_details['password'] = getpass.getpass()
                        password2 = getpass.getpass(force_str('Password (again): '))
                        if usr_details['password'] != password2:
                            self.stderr.write("Error: Your passwords didn't match.")
                            usr_details['password'] = None
                            continue
                    if usr_details['password'].strip() == '':
                        self.stderr.write("Error: Blank passwords aren't allowed.")
                        usr_details['password'] = None
                        continue
                #-----------------------#
                # end password creation #
                #-----------------------#

            #------------------#
            # End code we need #
            #------------------#


            # I LIKE THIS.
            except KeyboardInterrupt:
                self.stderr.write("\nOperation cancelled.")
                sys.exit(1)

            except NotRunningInTTYException:
                self.stdout.write(
                    "Superuser creation skipped due to not running in a TTY. "
                    "You can run `manage.py createsuperuser` in your project "
                    "to create one manually."
                )

        if usr_details['username']:
            # PUT THE LDAP SHIT IN HERE

            print "usermodel: "+str(self.UserModel)
            print "manager: "+str(self.UserModel._default_manager)
#           print "db_manager: "+str(self.UserModel._default_manager.db_manager(database)) # don't need this access
            print "user_data: "+str(user_data)

#            self.UserModel = mcUser.objects.createMcUser(user_data)
#            self.UserModel.save()
            # call mcUserManager.createsuperuser() directly instead.
            self.UserModel._default_manager.db_manager(database).create_superuser(usr_details)
            

            if verbosity >= 1:
                self.stdout.write("Superuser created successfully.")
