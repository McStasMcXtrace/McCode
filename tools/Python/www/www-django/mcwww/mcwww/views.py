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
from django.contrib.auth import authenticate, login, logout
from django.shortcuts import redirect
from django.conf.urls import include
from django.contrib.auth.decorators import login_required
#-------------#
# app imports #
#-------------#
from common import *
from mcUser.models import *
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
    for key,val in form.items():
        print key, ":", val
    nexturl = form.get('next', '/')
    UID = form.get('uid', '') or form.get('username', '')
    PW = form.get('password', '')
    if PW == None: return redirect('/login/')
    checker = mcBackend()
    print "UID:,",UID,", PW: ", PW,"\n\n"
    user = checker.authenticate(UID, PW)
    if user is None or not user.is_active:
        return redirect('/login/Invalid_credentials')
    else: 
        login(req, user)
        # checker.session_login(user)
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

