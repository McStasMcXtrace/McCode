#=================================#
# views.py                        #
# --------                        #
# - should really move the logon  #
#   view to mcuser                #
# -------------------             #
# Edited : Mark Lewis             #
#=================================#
import datetime
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

    # Check if correct credentials
    # - should put a msg to pass to redirect as login report
    if kwds['password'] == None: return redirect('/login/')
    objects = ModelBackend()
    #    user = User.objects.authenticate(kwds['username'], kwds['password'])
    user = objects.authenticate(kwds['username'], kwds['password'])
    #    user_list = objects.all()
    print "user_list:"
    print User.objects.all()
    print "username:%s, pwd:%s"%(kwds['username'], kwds['password'])
    
    # Check if user in group for redirect
    # - should put a msg to pass to redirect as login report
    group = user.groups.all().first()
    print "GROUP: %s"%(group)
    if group == None: return redirect('/login/')

    # Hack to open a job page on login
    first_login = user.date_joined == user.last_login
    print "user.date_joined == user.last_login => first_login:%s"%first_login
    if(first_login):
        from mcsimulator.models import Job, Simulation
        simulations = Simulation.objects
        sim = simulations.get(name__startswith=group)
        print "SIM:%s"%sim
        print "     id:        %s"%sim.id
        print "   name:        %s"%sim.name
        print "   simgroup:    %s"%sim.simgroup
        print "   displayname: %s"%sim.displayname
        print "   params:"
        for k,v in sim.params.items():
            print "     key:%s, val:%s"%(k,v)
        # got a simulation that is relevent to the user now build and save a job for them
        job_ref = "%s_%s_%s"%(datetime.datetime.now().strftime("%Y%m%d%H%M%S"),
                              sim.name(), user.name )
        Job.new(job_ref, sim, 0, 1000, 1000, sim.id ).save()
        # then pass this job to the redirect



        #    for m in [method for method in dir(OBJECT) if callable(getattr(OBJECT,method))]:
        #      if(m[0]!="_"):print m
        # End of Job ref hack

    if user is None or not user.is_active:
        return redirect('/login/Invalid_credentials')
    else: 
        user.backend = 'django.contrib.auth.backends.ModelBackend'
        login(req, user)
        return redirect(nexturl+job_ref)
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

