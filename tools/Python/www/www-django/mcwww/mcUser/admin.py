#=====================================#
# AdminSite Overwrite & mcUserAdmin   #
# -------------------                 #
# - Provide a new admin site to login #
#   using the LDAP login method.      #
# ------------------                  #
# Author: Mark Lewis                  #
#=====================================#
# system imports
# python imports
from functools import update_wrapper
# django imports - go through this and remove unecc ones, this needs to be trimmed
from django.http import Http404, HttpResponseRedirect
from django.views.decorators.csrf import csrf_protect
from django.views.decorators.cache import never_cache
from django.template.response import TemplateResponse
from django.db import models
from django.db.models.base import ModelBase
from django.apps import apps
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.core.urlresolvers import reverse, NoReverseMatch
from django.utils import six
from django.utils.text import capfirst
from django.utils.translation import ugettext_lazy, ugettext as _
from django.conf import settings
from django.conf.urls import patterns, include
from django.contrib import admin
from django.contrib.auth import REDIRECT_FIELD_NAME
from django.contrib.admin import ModelAdmin, actions, AdminSite
# app imports
from mcsimulator.models import Simulation, SimRun, Job
from mcUser.models import mcUser
#----------------------------------------------------------------------------------------------#
#==============#
# mcUser Admin # - the views will offer the change pwd etc as LDAP access necc.
#==============#
class mcUserAdmin(admin.ModelAdmin):
    exclude         = ('is_active')
    readonly_fields = ('uid')
    list_filter     = ('is_staff', 'is_active')
    def has_add_permission(self, reqwuest, obj=None):
        return False
    def __init__(self, *args, **kwargs):
        super(mcUserAdmin, self).__init__(*args, **kwargs)
    class Meta:
        ordering = ['username']
#==================#
# END mcUser Admin #
#==================#
#----------------------------------------------------------------------------------------------#
#===================================================================#
# Overlord
# --------
# site_title: Text to put at the end of each page's <title>.
# site_header: Text to put in each page's <h1>.
# index_title: Text to put at the top of the admin index page.
#
# Overwritten Methods      Ignored Methods
# -------------------      ---------------
#                          - register()
# - has_permission()       - unregister()
# - admin_view()*          - add_action()
# - get_urls()             - disable_action()
# - urls                   - get_action()
# - passwqord_change       - actions()
# - password_change_done   - __init__()
# - logout
# - login
# - index
# - app_index
# * = decorator
#
# use of admin_view
# -----------------
"""
Decorator to create an admin view attached to this ``AdminSite``. 
This wraps the view and provides permission checking by calling ``self.has_permission``.
You'll want to use this from within ``AdminSite.get_urls()``:

  class MyAdminSite(AdminSite):
    def get_urls(self):
    from django.conf.urls import patterns, url
    urls = super(MyAdminSite, self).get_urls()
    urls += patterns('',
                     url(r'^my_view/$', self.admin_view(some_view))
                     )
    return urls

By default, admin_views are marked non-cacheable using the
``never_cache`` decorator. If the view can be safely cached, set
cacheable=True.
"""
#===================================================================#
class Overlord(AdminSite):
    site_title = ugettext_lazy('McCode Admin')
    site_header = ugettext_lazy('McCode Administration')
    index_title = ugettext_lazy('Site administration')
    # NEED TO WORK ON THESE
    login_form = None                          # this is templated from the login page
    login_template = None
    logout_template = None
    # WORK ON LATER
    index_template = None                      
    app_index_template = None                  
    password_change_template = None
    password_change_done_template = None
    app_name = "McStas"
    
    def has_permission(self, request):
        """
        Returns True if the given HttpRequest has permission to view *at least one* page in the admin site.
        """
        return request.user.is_active and request.user.is_staff

    def admin_view(self, view, cacheable=False):
        def inner(request, *args, **kwargs):
            if not self.has_permission(request):
                if request.path == reverse('admin:logout', current_app=self.name):
                    index_path = reverse('admin:index', current_app=self.name)
                    return HttpResponseRedirect(index_path)
                # Inner import to prevent django.contrib.admin (app) from
                # importing django.contrib.auth.models.User (unrelated model).
                from django.contrib.auth.views import redirect_to_login
                return redirect_to_login(
                    request.get_full_path(),
                    reverse('admin:login', current_app=self.name)
                )
            return view(request, *args, **kwargs)
        if not cacheable:
            inner = never_cache(inner)
        # We add csrf_protect here so this function can be used as a utility
        # function for any view, without having to repeat 'csrf_protect'.
        if not getattr(view, 'csrf_exempt', False):
            inner = csrf_protect(inner)
        return update_wrapper(inner, view)

    def get_urls(self):
        from django.conf.urls import patterns, url, include
        from django.contrib.contenttypes import views as contenttype_views

        if settings.DEBUG: self.check_dependencies()

        def wrap(view, cacheable=False):
            def wrapper(*args, **kwargs):
                return self.admin_view(view, cacheable)(*args, **kwargs)
            return update_wrapper(wrapper, view)
        #---------------------------------------------------#
        # Admin-site-wide views                             # WE NEED TO LINK THIS WITH CORRECT LOGIN VIEWS TO BEGIN WITH.
        # ---------------------                             #
        # Add urls here, build the page and make the views. #
        #
        urlpatterns = patterns( '',
                                url(r'^$', wrap(self.index), name='index'),
                                url(r'^login/$', self.login, name='login'),
                                url(r'^login/try/$', 'mcwww.views.loginPOST', name='loginPOST'),
                                url(r'^logout/$', wrap(self.logout), name='logout'),
                                url(r'^password_change/$', wrap(self.password_change, cacheable=True), name='password_change'),
                                url(r'^password_change/done/$', wrap(self.password_change_done, cacheable=True), name='password_change_done'),
                                url(r'^jsi18n/$', wrap(self.i18n_javascript, cacheable=True), name='jsi18n'),
                                url(r'^r/(?P<content_type_id>\d+)/(?P<object_id>.+)/$', wrap(contenttype_views.shortcut), name='view_on_site'),
                            )
        # Add in each model's views, and create a list of valid URLS for the
        # app_index
        valid_app_labels = []
        for model, model_admin in six.iteritems(self._registry):
            print "model: ", model, ", model_admin: ", model_admin
            urlpatterns += patterns('',
                url(r'^%s/%s/' % (model._meta.app_label, model._meta.model_name), include(model_admin.urls))
            )
            if model._meta.app_label not in valid_app_labels: valid_app_labels.append(model._meta.app_label)
        #
        # End Admin-site-wide views
        #---------------------------------------------------#

        # If there were ModelAdmins registered, we should have a list of app
        # labels for which we need to allow access to the app_index view,
        if valid_app_labels:
            regex = r'^(?P<app_label>' + '|'.join(valid_app_labels) + ')/$'
            urlpatterns += patterns('',
                url(regex, wrap(self.app_index), name='app_list'),
            )
        return urlpatterns

    @property
    def urls(self):
        return self.get_urls(), self.app_name, self.name

    def each_context(self):
        """
        Returns a dictionary of variables to put in the template context for *every* page in the admin site.
        """
        return { 'site_title': self.site_title,
                 'site_header': self.site_header,
             }

    def password_change(self, request):
        super.password_change(request)

    def password_change_done(self, request, extra_context=None):        
        """
        Displays the "success" page after a password change.
        """
        super.password_change_done(request, extra_context)


    @never_cache
    def logout(self, request, extra_context=None):
        """
        Logs out the user for the given HttpRequest. This should *not* assume the user is already logged in.
        """
        from django.contrib.auth.views import logout
        defaults = { 'current_app': self.name,
                     'extra_context': dict(self.each_context(), **(extra_context or {})),
        }
        if self.logout_template is not None:
            defaults['template_name'] = self.logout_template
        return logout(request, **defaults)

    #======================================================================#
    # login                                                                #
    # -----                                                                #
    # Displays the login form for the given HttpRequest.                   #
    # if Already logged-in, redirect to admin index                        #
    #                                                                      #
    # Since this module gets imported in the application's root package,   # - YEAH, THIS IS WHY, IT IMPORTS USER
    # it cannot import models from other applications at the module level, #   WHY IS THIS NOT IN THE DOCUMENTATION???
    # and django.contrib.admin.forms eventually imports User.              #
    @never_cache
    def login(self, request, extra_context=None):
        # CHASING LOGIN OF ADMIN
        from req_logs.logger import log_this
        log_this(request, 'admin_login_check', [request.method, self.has_permission(request), REDIRECT_FIELD_NAME])
        print "\n"
        print "req method: %s"%request.method
        print "permission: %s"%self.has_permission(request)
        print "\n"
        #     request.method = POST        -> first condition not made.
        #     self.has_permission = False! -> second condition not made.
        # this is for already logged in users I think.
        # END OF CHASING
        if request.method == 'GET': # and self.has_permission(request):
            print "in the if#1"
            index_path = reverse('admin:index', current_app=self.name)
            return HttpResponseRedirect(index_path)

        from django.contrib.auth.views import login # login functionality import - just import correctly?
        from django.contrib.admin.forms import AdminAuthenticationForm
        
        context = dict(self.each_context(),
                       title=_('Log in'),
                       app_path=request.get_full_path(),
                   )
        # MORE LOGIN CHASING
        print "REDIRECT_FIELD_NAME: ", REDIRECT_FIELD_NAME
        # END OF CHASING
        if (REDIRECT_FIELD_NAME not in request.GET
            and
            REDIRECT_FIELD_NAME not in request.POST):
            print "in the if#2"
            context[REDIRECT_FIELD_NAME] = request.get_full_path()
        
        context.update(extra_context or {})
        defaults = { 'extra_context': context,
                     'current_app': self.name,
                     'authentication_form': self.login_form or AdminAuthenticationForm,
                     'template_name': self.login_template or 'admin/login.html',
                 }

        # this is where the LDAP login should be implemented.

        return login(request, **defaults)
    
    @never_cache
    def index(self, request, extra_context=None):
        """
        Displays the main admin index page, which lists all of the installed
        apps that have been registered in this site.
        """
        app_dict = {}
        user = request.user
        for model, model_admin in self._registry.items():
            app_label = model._meta.app_label
            has_module_perms = user.has_module_perms(app_label)

            if has_module_perms:
                perms = model_admin.get_model_perms(request)

                # Check whether user has any perm for this module.
                # If so, add the module to the model_list.
                if True in perms.values():
                    info = (app_label, model._meta.model_name)
                    model_dict = { 'name': capfirst(model._meta.verbose_name_plural),
                                   'object_name': model._meta.object_name,
                                   'perms': perms,
                               }
                    if perms.get('change', False):
                        try:
                            model_dict['admin_url'] = reverse('admin:%s_%s_changelist' % info, current_app=self.name)
                        except NoReverseMatch:
                            pass
                    if perms.get('add', False):
                        try:
                            model_dict['add_url'] = reverse('admin:%s_%s_add' % info, current_app=self.name)
                        except NoReverseMatch:
                            pass
                    if app_label in app_dict:
                        app_dict[app_label]['models'].append(model_dict)
                    else:
                        app_dict[app_label] = { 'name': apps.get_app_config(app_label).verbose_name,
                                                'app_label': app_label,
                                                'app_url': reverse('admin:app_list', kwargs={'app_label': app_label}, current_app=self.name),
                                                'has_module_perms': has_module_perms,
                                                'models': [model_dict],
                                            }

        # Sort the apps alphabetically.
        app_list = list(six.itervalues(app_dict))
        app_list.sort(key=lambda x: x['name'].lower())

        # Sort the models alphabetically within each app.
        for app in app_list:
            app['models'].sort(key=lambda x: x['name'])

        context = dict( self.each_context(),
                        title=self.index_title,
                        app_list=app_list,
                    )
        context.update(extra_context or {})
        return TemplateResponse( request,
                                 self.index_template or 'admin/index.html',
                                 context,
                                 current_app=self.name )

    def app_index(self, request, app_label, extra_context=None):
        user = request.user
        app_name = apps.get_app_config(app_label).verbose_name
        has_module_perms = user.has_module_perms(app_label)
        if not has_module_perms:
            raise PermissionDenied
        app_dict = {}
        for model, model_admin in self._registry.items():
            if app_label == model._meta.app_label:
                perms = model_admin.get_model_perms(request)

                # Check whether user has any perm for this module.
                # If so, add the module to the model_list.
                if True in perms.values():
                    info = (app_label, model._meta.model_name)
                    model_dict = {
                        'name': capfirst(model._meta.verbose_name_plural),
                        'object_name': model._meta.object_name,
                        'perms': perms,
                    }
                    if perms.get('change'):
                        try:
                            model_dict['admin_url'] = reverse('admin:%s_%s_changelist' % info, current_app=self.name)
                        except NoReverseMatch:
                            pass
                    if perms.get('add'):
                        try:
                            model_dict['add_url'] = reverse('admin:%s_%s_add' % info, current_app=self.name)
                        except NoReverseMatch:
                            pass
                    if app_dict:
                        app_dict['models'].append(model_dict),
                    else:
                        # First time around, now that we know there's
                        # something to display, add in the necessary meta
                        # information.
                        app_dict = {
                            'name': app_name,
                            'app_label': app_label,
                            'app_url': '',
                            'has_module_perms': has_module_perms,
                            'models': [model_dict],
                        }
        if not app_dict:
            raise Http404('The requested admin page does not exist.')
        # Sort the models alphabetically within each app.
        app_dict['models'].sort(key=lambda x: x['name'])
        context = dict(self.each_context(),
            title=_('%(app)s administration') % {'app': app_name},
                       app_list=[app_dict],
                       app_label=app_label,
                   )
        context.update(extra_context or {})

        return TemplateResponse(request,
                                self.app_index_template or ['admin/%s/app_index.html' % app_label,
                                                            'admin/app_index.html'
                                                        ],
                                context,
                                current_app=self.name)

#===================================#
# Setting up and registering Admins #
#===================================#
admin_site = Overlord(name='overlord_admin')
for model in (Simulation, SimRun, Job, mcUser):
    admin_site.register(model)
