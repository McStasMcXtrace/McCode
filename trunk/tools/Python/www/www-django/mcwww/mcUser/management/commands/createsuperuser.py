#==========================================#
# createsuperuser.py                       #
# ------------------                       #
# Management utility to create superusers. #
# Templated from django.auth commands      #
# ---------------                          #
# Edited by: Mark Lewis                    #
# 
# FUTURE IMPROVEMENTS:
# - Move a lot more from user/superuser
#   creation into the creation_helper.
#   
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
from mcUser.management.LDAP.LDAPUserCreation import *
from mcUser.models import *
from creation_helpers import check_LDAP_perms, duplicate_user_check, get_email

class NotRunningInTTYException(Exception):
    pass

class Command(BaseCommand):
    def __init__(self, *args, **kwargs):
        # Options are defined in an __init__ method to support swapping out
        super(Command, self).__init__(*args, **kwargs)

        self.User = mcUser()
        # this is always going to be mcUser.uid, keep in to ensure model updatable.
        self.username_field = mcUser._meta.get_field(mcUser.USERNAME_FIELD)

        self.option_list = BaseCommand.option_list + (make_option('--%s' % mcUser.USERNAME_FIELD, # this is mcUser::uid
                                                                  dest=mcUser.USERNAME_FIELD, 
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
                                                                        ' not be able to log in until they\'re given a valid password.' % mcUser.USERNAME_FIELD)
                                                                  ),
                                                      make_option('--database',                           # this is mcwww.sqlite, but std way to treat.
                                                                  action='store', 
                                                                  dest='database',
                                                                  default=DEFAULT_DB_ALIAS, 
                                                                  help='Specifies the database to use. Default is "default".'
                                                                  ),
                                                      ) + tuple(make_option('--%s' % field,                    # this will be used because it's an easy way to
                                                                            dest=field,                        # make a form from the admin site....don't hate me, future mark :(
                                                                            default=None,
                                                                            help='Specifies the %s for the superuser.' % field
                                                                            ) for field in mcUser.REQUIRED_FIELDS
                                                                )
        option_list = BaseCommand.option_list
        help = 'Used to create a superuser.'
        
    def execute(self, *args, **options):
        self.stdin = options.get('stdin', sys.stdin)
        return super(Command, self).execute(*args, **options)


    #==========#
    # handle() #
    # -------- #
    #==========#
    def handle(self,  *args, **options):
        LDAP_admin_dn = raw_input('Enter your LDAP authentication cn (not your uid): ')
        check_LDAP_perms(LDAP_admin_dn)
        usr_details = {'staff':    True,
                       'username': None,
                       'password': None}

        interactive = options.get('interactive')
        verbosity = int(options.get('verbosity', 1))
        database = options.get('database')
        #------------------------#
        # usr_details Form setup # - NOT DELETING THIS FOR FUTURE ADMIN SITE VIEW USE.
        # 
        #        if not interactive:
        #            print "in here."
        #            try:
        #                if not usr_details['username']:
        #                    raise CommandError("You must use --%s with --noinput." % mcUser.USERNAME_FIELD)
        #                usr_details['username'] = self.username_field.clean(usr_details['username'], None)      
        #                for field_name in mcUser.REQUIRED_FIELDS:
        #                    if options.get(field_name):
        #                        field = mcUser._meta.get_field(field_name)
        #                        usr_details[field_name] = field.clean(options[field_name], None)
        #                    else:
        #                        raise CommandError("You must use --%s with --noinput." % field_name)
        #            except exceptions.ValidationError as e:
        #                raise CommandError('; '.join(e.messages))
        #
        # End Form setup #
        #----------------#
        #        else:
        default_username = get_default_username()
        try:
            if hasattr(self.stdin, 'isatty') and not self.stdin.isatty(): raise NotRunningInTTYException("Not running in a TTY")
            # Username creation
            while not usr_details['username']:
                input_msg = 'Username'
                if not usr_details['username']:
                    if default_username:
                        input_msg = input_msg + " (leave blank to use '%s')" % default_username
                    raw_value = input(force_str('%s: ' % input_msg))
                if default_username and raw_value == '': raw_value = default_username
                try:
                    usr_details['username'] = raw_value
                except exceptions.ValidationError as e:
                    self.stderr.write("Error: %s" % '; '.join(e.messages))
                    usr_details['username'] = None
                    continue
            # Check duplicates, create uid
            duplicate_user_check(usr_details)
            # email creation
            get_email(usr_details)
            # Password creation
            while not usr_details['password']:
                if not usr_details['password']:                        
                    usr_details['password'] = getpass.getpass()
                    password2 = getpass.getpass(force_str('Password (again): '))
                    if usr_details['password'] != password2:
                        self.stderr.write("Error: Your passwords didn't match.")
                        usr_details['password'] = None
                        continue
                elif usr_details['password'].strip() == '':
                    self.stderr.write("Error: Blank passwords aren't allowed.")
                    usr_details['password'] = None
                    continue
                else: encrypt_password(usr_details)

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
            LDAP_admin_pw = getpass.getpass('Enter your LDAP authentication pwd: ')
            entity = LDAPUserCreation(usr_details)
            print "Calling: processLDIF(", LDAP_admin_dn, ",", LDAP_admin_pw, ")"
            entity.processLDIF(LDAP_admin_dn, LDAP_admin_pw)
            print "LDAP user added, adding django user."
            mcUser.objects.create_superuser(usr_details)
            print "Super user created."

