#=======================================================================#
# Django micro framework                                                #
# ----------------------                                                #
# Template Author: Andrew Pendleton                                     #
# ------------------------                                              #
# https://github.com/apendleton/djmicro/blob/master/djmicro.py          #
# http://softwaremaniacs.org/blog/2011/01/07/django-micro-framework/en/ #
# ------------------------                                              #
# Server Setup : Standalone mcUploader App                              #
# ------------------------                                              #
# Editor: Mark Lewis                                                    #
#=======================================================================#

#---------------------------------------#
# _base_module = 'private' global       #
#                id's the server module #
#                dangerous in release?  #
#---------------------------------------#
import os
_base_module = None

#===========================================================#
# configure                                                 #
# ---------                                                 #
# Sets _base_module                                         #
# Sets up options                                           #
# Configures settings       <-- app specific tweaks here    #
# Instantiates urlpatterns  <-- this will be filled up with #
#                               sites by 'route'            #
#===========================================================#
def configure(options={}, module=None):
    if not module:
        # hack to figure out where we were called from
        import sys, inspect
        module = sys.modules[inspect.stack()[1][0].f_locals['__name__']]
    # settings
    from django.conf import settings
    if not settings.configured:
        root_path = lambda s: os.path.join(os.path.dirname(__file__), 
                                           s).replace('\\','/')
        print 'Configuring Settings.'
        opts = dict(
            STATIC_URL = root_path('static'),
            DEBUG = True,
            ROOT_URLCONF = module.__name__,
            TEMPLATE_DIRS = [os.path.dirname(module.__file__),
                             root_path('templates')],
            INSTALLED_APPS = ['mcUser'],
            STATICFILES_DIRS = (root_path('static/'),
                                root_path('out/'),
                                root_path('templates/js_css')
                ),
            LDIF_DIR = root_path('management/LDAP/LDIFs/temp/')
            )
        opts.update(options)
        settings.configure(**opts)
    # urls
    from django.conf.urls.defaults import patterns
    module.urlpatterns = patterns('')
    global _base_module
    _base_module = module

#===========================================#
# route                                     #
# -----                                     #
# Decorator function. Parses the function   #
# it decorates and sets it as the view      #
# for the url passed to it as argument.     #
# kwargs can be anything that nromal django #
# urls take.                                #
#===========================================#
def route(*args, **kwargs):
    def add_route(view):
        from django.conf.urls.defaults import patterns, url
        _base_module.urlpatterns += patterns('',
                                             url(args[0], view, *args[1:], **kwargs)
                                             )
        return view
#    show_patterns()
    return add_route

#======================#
# print available urls #
#======================#
def show_patterns():
    print str(_base_module.urlpatterns)

#==================================#
# run                              #
# ---                              #
# Allows operation as ./manage.py  #
# Call from main server class      #
# with manage commands.            #
#==================================#
def run():
    from django.core.management import execute_from_command_line
    execute_from_command_line()
 
