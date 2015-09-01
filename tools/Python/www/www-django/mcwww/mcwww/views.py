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

