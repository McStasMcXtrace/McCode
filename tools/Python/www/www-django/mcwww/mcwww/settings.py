import os
PROJECT_PATH = os.path.realpath(os.path.dirname(os.path.dirname(__file__)))
#===============#
# McUser CONFIG #
#===============#
# LOGIN_URL = './login',
LOGIN_URL = '/login'
AUTHENTICATION_BACKENDS=('models.mcBackend')
AUTH_USER_MODEL        =('models.mcUser')
SESSION_COOKIE_AGE              = 10*60
SESSION_EXPIRE_AT_BROWSER_CLOSE = True
SESSION_SAVE_EVERY_REQUEST      = True
LDIF_DIR                        = 'mcUser/management/LDAP/LDIFs/temp/'
# STATIC_URL                      = root_path('static')
# ROOT_URLCONF                    = module.__name__
#TEMPLATE_DIRS                   = [os.path.dirname(module.__file__),
#                                   root_path('templates')]
# SESSION_ENGINE                  = 'mcUser.mcSession'
# SESSION_FILE_PATH               = './sqlite3_DB/session_data'
#============#
# McUser END #
#============#

#============================#
# BEGIN McStas configuration #  What you need is probably in this block!
#============================#
# IMAGE_FORMAT is either 'gif' or 'png' - R-based plotter only supports png
IMAGE_FORMAT = 'png'
# Use MPI to utilise N processors (disabled when 0)
MPI_NP=0
# Maximum number of neutrons allowed (ray samples / ncount)
MAX_RAY_SAMPLES = 10000000
# Maximum number of scan points allowed (npoints)
MAX_SCAN_POINTS = 1000
# The line below would create a link in the simulation folder
# named 'datafiles' to the path in 'sim/datafiles'
# (relative to the webapp folder)
DATA_FILES = ('sim/datafiles',)
#==========================#
# END McStas configuration #
#==========================#

#===============#
# Django Config #
#===============#
DEBUG = False # True # - put this to be true only when dev'ing 
TEMPLATE_DEBUG = DEBUG
ROOT_URLCONF = 'mcwww.urls'

INSTALLED_APPS = (
    'mcsimulator',
    'mcwww',
    'mcUser',

    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.sites',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    # Uncomment the next line to enable the admin:
    'django.contrib.admin',
    # Uncomment the next line to enable admin documentation:
    # 'django.contrib.admindocs',
)
# Python dotted path to the WSGI application used by Django's runserver.
WSGI_APPLICATION = 'mcwww.wsgi.application'

#-----------------#
# DATABASE CONFIG #
#-----------------#
ADMINS = (
    ('Mark Lewis', 'lewis@fysik.dtu.dk'),
)
MANAGERS = ADMINS
DATABASES = {
    'default': {
        # Add 'postgresql_psycopg2', 'mysql', 'sqlite3' or 'oracle'.
        'ENGINE': 'django.db.backends.sqlite3',
        'NAME': './DB/mcwww.sqlite3',    # Or path to database file if using sqlite3.
        'USER': '',                      # Not used with sqlite3.
        'PASSWORD': '',                  # Not used with sqlite3.
        'HOST': '',                      # Set to empty string for localhost. Not used with sqlite3.
        'PORT': '',                      # Set to empty string for default. Not used with sqlite3.
    }
}
#---------------#
# END DATABASES #
#---------------#

#-------------#
# FILE ACCESS #
#-------------#
# Absolute path to the directory static files should be collected to.
# Don't put anything in this directory yourself; store your static files
# in apps' "static/" subdirectories and in STATICFILES_DIRS.
# Example: "/home/media/media.lawrence.com/static/"
STATIC_ROOT = ''
# URL prefix for static files.
# Example: "http://media.lawrence.com/static/"
STATIC_URL = '/static/'
# Additional locations of static files
STATICFILES_DIRS = (
    # Put strings here, like "/home/html/static" or "C:/www/django/static".
    # Always use forward slashes, even on Windows.
    # Don't forget to use absolute paths, not relative paths.
    PROJECT_PATH + '/static/',
    PROJECT_PATH + '/out/'
)
# List of finder classes that know how to find static files in
# various locations.
STATICFILES_FINDERS = (
    'django.contrib.staticfiles.finders.FileSystemFinder',
    'django.contrib.staticfiles.finders.AppDirectoriesFinder',
#    'django.contrib.staticfiles.finders.DefaultStorageFinder',
)

TEMPLATE_DIRS = (
    # Put strings here, like "/home/html/django_templates" or "C:/www/django/templates".
    # Always use forward slashes, even on Windows.
    # Don't forget to use absolute paths, not relative paths.
    PROJECT_PATH + '/templates/'
)

#============================#
# END (USEFUL) DJANGO CONFIG #
#============================#

#=====================#
# FUTURE UPLOADER USE #
#=====================#
# Absolute filesystem path to the directory that will hold user-uploaded files.
# Example: "/home/media/media.lawrence.com/media/"
MEDIA_ROOT = ''
# URL that handles the media served from MEDIA_ROOT. Make sure to use a
# trailing slash.
# Examples: "http://media.lawrence.com/media/", "http://example.com/media/"
MEDIA_URL = ''
#==============#
# END UPLOADER #
#==============#

#-----------#
# STD STUFF #
#-----------#
# Local time zone for this installation. Choices can be found here:
# http://en.wikipedia.org/wiki/List_of_tz_zones_by_name
# although not all choices may be available on all operating systems.
# In a Windows environment this must be set to your system time zone.
TIME_ZONE = 'Europe/Copenhagen'

# Language code for this installation. All choices can be found here:
# http://www.i18nguy.com/unicode/language-identifiers.html
LANGUAGE_CODE = 'en-gb'

SITE_ID = 1

# If you set this to False, Django will make some optimizations so as not
# to load the internationalization machinery.
USE_I18N = True

# If you set this to False, Django will not format dates, numbers and
# calendars according to the current locale.
USE_L10N = True

# If you set this to False, Django will not use timezone-aware datetimes.
USE_TZ = True

# Make this unique, and don't share it with anybody.
SECRET_KEY = 'gaeh@565h%=7)gw#625*ag82am#*55xnb40xa769yaxq-^ukj*'

# List of callables that know how to import templates from various sources.
TEMPLATE_LOADERS = (
    'django.template.loaders.filesystem.Loader',
    'django.template.loaders.app_directories.Loader',
#     'django.template.loaders.eggs.Loader',
)

MIDDLEWARE_CLASSES = (
    'django.middleware.common.CommonMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    # Uncomment the next line for simple clickjacking protection:
    # 'django.middleware.clickjacking.XFrameOptionsMiddleware',
)

# A sample logging configuration. The only tangible logging
# performed by this configuration is to send an email to
# the site admins on every HTTP 500 error when DEBUG=False.
# See http://docs.djangoproject.com/en/dev/topics/logging for
# more details on how to customize your logging configuration.
LOGGING = {
    'version': 1,
    'disable_existing_loggers': False,
    'filters': {
        'require_debug_false': {
            '()': 'django.utils.log.RequireDebugFalse'
        }
    },
    'handlers': {
        'mail_admins': {
            'level': 'ERROR',
            'filters': ['require_debug_false'],
            'class': 'django.utils.log.AdminEmailHandler'
        }
    },
    'loggers': {
        'django.request': {
            'handlers': ['mail_admins'],
            'level': 'ERROR',
            'propagate': True,
        },
    }
}

#MIDDLEWARE_CLASSES = (
#    'django.contrib.sessions.middleware.Session.Middleware',
#)



