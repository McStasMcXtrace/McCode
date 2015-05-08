#---------------------------------------------------------------------------------------------------#
# Logging - maybe this can be used with a cron to drag data from Moodle (THOUGHTS FOR LATER)
# -------
''' 
 A sample logging configuration. The only tangible logging performed by this configuration is to 
 send an email to the site admins on every HTTP 500 error when DEBUG=False.
 See http://docs.djangoproject.com/en/dev/topics/logging for more details on how to customize your 
 logging configuration. 
'''
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

#---------------------------------------------------------------------------------------------------#
# McStas Config
# -------------
# - IMAGE_FORMAT : {'gif' or 'png'} - R-plot supports png
# - MPI_NP : N processors (disabled when 0)
# - Max neutrons emitted (ray samples / ncount)
# - Max scan points used (npoints)
# - Extra datafiles/directories needed by the simulations (soft/symbolic link before running sim)
IMAGE_FORMAT = 'png'
MPI_NP=0
MAX_RAY_SAMPLES = 10000000
MAX_SCAN_POINTS = 1000
DATA_FILES = ('sim/datafiles',)

#---------------------------------------------------------------------------------------------------#
# Lang and Formatting
# -------------------
# - USE_I18N      : internationalise machinery
# - USE_L10N      : locale formatting (dates, numbers and calendars)
# - USE_TZ        : timezone-aware datetimes.
# - LANGUAGE_CODE : language settings
LANGUAGE_CODE = 'en-gb'
USE_I18N = True
USE_L10N = True
USE_TZ = True

# - TEMPLATE_LOADERS    : List of callables that know how to import templates from various sources.
# ------------------
MEDIA_ROOT = ''
MEDIA_URL = ''

STATIC_ROOT = ''
STATICFILES_FINDERS = (
    'django.contrib.staticfiles.finders.FileSystemFinder',
    'django.contrib.staticfiles.finders.AppDirectoriesFinder',
#    'django.contrib.staticfiles.finders.DefaultStorageFinder',
)

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


