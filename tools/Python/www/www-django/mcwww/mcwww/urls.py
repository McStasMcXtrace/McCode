from django.conf.urls import patterns, include, url

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
from django.conf.urls import patterns, include
# from mcUser.admin import admin_site
admin.autodiscover()

RUNREF_RE='[\w\.:-]+'

urlpatterns = patterns(
    '',

    url(r'^login/$',     'mcwww.views.login_form' , name='login_form'),
    url(r'^login/try/$', 'mcwww.views.loginPOST',   name='loginPOST'),
    url(r'^login/Invalid_credentials/$', 'mcwww.views.login_form',   name='login_form'),
    url(r'^logout/$',    'mcwww.views.logout_user', name='logout_user'),
    
    url(r'^sim/latest/', 'mcsimulator.views.latest', name='latest'),

    url(r'^$', 'mcsimulator.views.home', name='home'),

    url(r'^job/(?P<jobref>\w+)/$',        'mcsimulator.views.configure',     name='configure'),
    url(r'^job/update/(?P<jobref>\w+)/$', 'mcsimulator.views.configurePOST', name='configurePOST'),

    url(r'^sim/(?P<jobref>\w+)/$', 'mcsimulator.views.simulatePOST', name='simulatePOST'),

    url(r'^status/(?P<runref>'+RUNREF_RE+')/$', 'mcsimulator.views.status', name='status'),
    url(r'^sim/status/(?P<runref>'+RUNREF_RE+')/$', 'mcsimulator.views.status', name='status'),
    url(r'^sim/status/(?P<runref>'+RUNREF_RE+')/(?P<compN>.+)', 'mcsimulator.views.status', name='status'),

    url(r'^plot/(?P<runref>'+RUNREF_RE+')/(?P<name>.+)', 'mcsimulator.views.show_plot', name='show_plot'),

    url(r'^doc/(?P<instr>\w+)', 'mcsimulator.views.documentation', name='documentation'),


    # url(r'^mcwww/', include('mcwww.foo.urls')),

    # Uncomment the admin/doc line below to enable admin documentation:
    # url(r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the std django admin:
    url(r'^admin/', include(admin.site.urls)),

    # this line referes to the new admin site for login purposes.
    #    url(r'^admin/', include(admin_site.urls)),
    
)
