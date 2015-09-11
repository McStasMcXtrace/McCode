# REMEBER PEBCAK AND Id-10T errors.
import os
PROJECT_PATH = os.path.realpath(os.path.dirname(os.path.dirname(__file__)))
#===============#
# McUser CONFIG #
#===============#
LOGIN_URL                       = '/login'
# AUTHENTICATION_BACKENDS         = ('mcUser.models.mcBackend',)
# AUTH_USER_MODEL                 = ('mcUser.mcUser')
# AUTHENTICATION_BACKENDS         = ('auth.RemoteUserBackend',)
AUTH_USER_MODEL                 = ('auth.User')
SESSION_COOKIE_AGE              = 10*60
SESSION_EXPIRE_AT_BROWSER_CLOSE = True
SESSION_SAVE_EVERY_REQUEST      = True
LDIF_DIR                        = 'mcUser/management/LDAP/LDIFs/temp/'                   #   _______________________
SESSION_FILE_PATH               = './DB/session_data'                                    #  /                       This is actually important.
SESSION_ENGINE                  = "django.contrib.sessions.backends.file"                # v
SESSION_SERIALIZER              = 'django.contrib.sessions.serializers.PickleSerializer' # REGENERATE A DECENT SECRET KEY BECAUSE OF THIS... CAN WE CHANGE IT ON SERVER REBOOTS?
#============#
# McUser END #
#============#

#============================#
# BEGIN McStas configuration #
#============================#
IMAGE_FORMAT    = 'png'              # IMAGE_FORMAT is either 'gif' or 'png' - R-based plotter only supports png
MPI_NP          = 0                  # Use MPI to utilise N processors (disabled when 0)
MAX_RAY_SAMPLES = 10000000           # Maximum number of neutrons allowed (ray samples / ncount)
MAX_SCAN_POINTS = 1000               # Maximum number of scan points allowed (npoints)
DATA_FILES      = ('sim/datafiles',) # performs: ln <simulation folder>/datafiles -> 'sim/datafiles'  (relative to the webapp folder)
#==========================#
# END McStas configuration #
#==========================#

#===============#
# Django Config #
#===============#
# DJANGO_SETTINGS_MODULE = 'mcwww/settings.py'
SECRET_KEY     = 'gaeh@565h%=7)gw#625*ag82am#*55xnb40xa769yaxq-^ukj*'                    # THIS SHOULD BE SECRET! REGENERATE IT A LOT BECAUSE OF THE SERIALIZER USED. ----^
DEBUG          =  True # False                                                           <----  THIS NEEDS TO BE SET TO FALSE. (css broken if false!!!!)
TEMPLATE_DEBUG = DEBUG
INSTALLED_APPS = (
    # mcApps #
    # ------ #
    'mcsimulator',
    'mcwww',
    #    'mcUser',

    # django Apps #
    # ----------- #
    # Admin Site
    #    'django.contrib.admin',
    'django.contrib.admin.apps.SimpleAdminConfig',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    # Std
    'django.contrib.sessions',
    'django.contrib.sites',
    'django.contrib.messages',
    'django.contrib.staticfiles',

    # 'django.contrib.admindocs',
)
TEMPLATE_CONTEXT_PROCESSORS = (
    # Admin Site
    'django.contrib.messages.context_processors.messages',
    'django.contrib.auth.context_processors.auth',
    )
MIDDLEWARE_CLASSES = (
    'django.middleware.common.CommonMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    # Admin Site #
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    # 'django.middleware.clickjacking.XFrameOptionsMiddleware',     # Uncomment for clickjack protection
)

#-----------------#
# DATABASE CONFIG #
#-----------------#
ADMINS     = ( 
    ('Peter Willendrup', 'pkwi@fysik.dyu.dk'),
    ('Mark Lewis', 'lewis@fysik.dtu.dk'),
    )
MANAGERS   = ADMINS
DATABASES  = {                                            # keys not read by sqlite removed.
    'default': { 'ENGINE': 'django.db.backends.sqlite3',
                 'NAME': './DB/mcwww.sqlite3',            # (sqlite3: NAME = path to db file)
                 }
    }
#---------------#
# END DATABASES #
#---------------#

#--------------#
# FILE SERVING # 
#--------------#
WSGI_APPLICATION = 'mcwww.wsgi.application' # path to WSGI app used by runserver.
ROOT_URLCONF     = 'mcwww.urls'             # File holding URLs to be served.
TEMPLATE_DIRS    = (                        # Absolute paths to template.htmls
    PROJECT_PATH + '/templates/',
    # Admin Site Templates #
    # -------------------- #
    PROJECT_PATH + '/templates/admin',
    #PROJECT_PATH + '/templates/mcUser',
    #PROJECT_PATH + '/templates/mcsimulator',
    #PROJECT_PATH + '/templates/mcwww',
    
    )
TEMPLATE_LOADERS = (                        # Callables that import templates.
    'django.template.loaders.filesystem.Loader',
    'django.template.loaders.app_directories.Loader',
    #     'django.template.loaders.eggs.Loader',
    )
STATIC_ROOT      = ''                       # Store custom static files in apps/static/ subdirectories and in STATICFILES_DIRS.
STATIC_URL       = '/static/'               # URL prefix for static files.
STATICFILES_DIRS = (                        # Absolute paths of static files.
    PROJECT_PATH + '/static/',
    PROJECT_PATH + '/out/'
)
STATICFILES_FINDERS = (                     # Classes that find static files.
    'django.contrib.staticfiles.finders.FileSystemFinder',
    'django.contrib.staticfiles.finders.AppDirectoriesFinder',
#    'django.contrib.staticfiles.finders.DefaultStorageFinder',
)
#------------------#
# END FILE SERVING # 
#------------------#
#============================#
# END (USEFUL) DJANGO CONFIG #
#============================#

#=====================#
# FUTURE UPLOADER USE # : Another File Serving Section, essentially.
#=====================#
MEDIA_ROOT = ''       # Absolute path to user-uploaded directory
MEDIA_URL  = ''        # URL that serves media from MEDIA_ROOT, inc trailing slash.
#==============#
# END UPLOADER #
#==============#

#-----------#
# STD STUFF #
#-----------#
TIME_ZONE     = 'Europe/Copenhagen'  # Local time zone. See http://en.wikipedia.org/wiki/List_of_tz_zones_by_name
LANGUAGE_CODE = 'en-gb'              # Language code. See http://www.i18nguy.com/unicode/language-identifiers.html
SITE_ID       = 1
USE_I18N      = True                 # loading internationalization machinery. (default -> true, optimised -> false... don't change)
USE_L10N      = True                 # locale date formatting
USE_TZ        = True                 # timezone-aware datetimes.
#----------------#
# logging config #
#----------------#
# this config:
#   filter
#    DEBUG=False.
#   handled by
#    email site admins 
#   perform on
#    HTTP 500 error 
LOGGING = { # See http://docs.djangoproject.com/en/dev/topics/logging 
    'version': 1,
    'disable_existing_loggers': False,
    'filters': {'require_debug_false':  {'()': 'django.utils.log.RequireDebugFalse' 
                                         }
                },
    'handlers': {'mail_admins': {'level': 'ERROR',
                                 'filters': ['require_debug_false'],
                                 'class': 'django.utils.log.AdminEmailHandler'
                                 }
                 },
    'loggers': {'django.request': {'handlers': ['mail_admins'],
                                   'level': 'ERROR',
                                   'propagate': True,
                                   },
                }
    }
