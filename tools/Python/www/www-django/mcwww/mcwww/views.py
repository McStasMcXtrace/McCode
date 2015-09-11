#=================================#
# views.py                        #
# --------                        #
# - should really move the logon  #
#   view to mcuser                #
# -------------------             #
# Edited : Mark Lewis             #
#=================================#
#------------------#
# django imports   #
#------------------#
from django.contrib.auth import login, logout, authenticate
from django.contrib.auth.backends import ModelBackend, RemoteUserBackend
from django.shortcuts import redirect
from django.conf.urls import include
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
#-------------#
# app imports #
#-------------#
from common import *
# from mcUser.models import *
from common import templated
#---------------#
#  login_form() #
#---------------#
@templated('mcwww/login')
def login_form(req):
    return dict(next = req.GET.get('next', '/'))
#----------------------------------#
# loginPOST                        # # couldn't find how the username variable is set in the admin loging form
# ---------                        # # not happy about this hack to requery the dict if it's not got uid key.
# Processes POST form passed to it #
# from login.html                  #
# authenticates user (using LDAP   #
# DB)                              # 
# if successful, logs user in      #
# if unsucessful returns login     #
#----------------------------------#
def loginPOST(req):
    form = req.POST
    nexturl = form.get('next', '/')
    kwds = {}
    kwds['username'] = form.get('uid', '') or form.get('username', '')
    kwds['password'] = form.get('password', '')
    if kwds['password'] == None: return redirect('/login/')
    objects = ModelBackend()
    user = objects.authenticate(kwds['username'], kwds['password'])
    
    # Hack to open a job page on login
    from mcsimulator.models import Job, Simulation
    
    creation   = user.date_joined
    last_login = user.last_login
    first_login = last_login == creation
    print "created: %s, last_login: %s"%(creation, last_login)

    group = user.groups.all().first()
    print "GROUP: %s"%(group)
    simulations = Simulation.objects
    sim = simulations.get(name__startswith=group)
    print "SIM:%s"%sim
    print "   name:        %s"%sim.name
    print "   simgroup:    %s"%sim.simgroup
    print "   displayname: %s"%sim.displayname
    print "   params:      %s"%sim.params
    for k,v in sim.params.items():
        print "     key:%s, val:%s"%(k,v)
    # got a simulation that is relevent to the user now build and save a job for them
        
#    job = Job::new()

    # then pass this job to the redirect



    #    for m in [method for method in dir(OBJECT) if callable(getattr(OBJECT,method))]:
    #      if(m[0]!="_"):print m
    # End of Job ref hack

    if user is None or not user.is_active:
        return redirect('/login/Invalid_credentials')
    else: 
        user.backend = 'django.contrib.auth.backends.ModelBackend'
        login(req, user)
        return redirect(nexturl)
#----------------------------------------------------#
#  logout_user()                                     #
# ===============                                    #
# called from configure.html|status.html -> urls.py  #
# - use django authenticate tools to logout user ;   #
# - send to login.html ;                             #
#----------------------------------------------------#
def logout_user(req):
    if req.user is not None:
        logout(req)
    return redirect('login_form')

