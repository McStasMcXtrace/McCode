#--------------------------#
# Edited : Mark Lewis 2015 #
#--------------------------#
#------------------#
# django imports   #
#------------------#
# from django.contrib.auth import authenticate, login, logout
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
# loginPOST                        #
# ---------                        #
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
    UID = form.get('uid', '')
    PW = form.get('password', '')
    if PW == None: return redirect('/login/')

    checker = mcBackend()
    user = checker.authenticate(UID, PW)
    print "user:", user
    if user is None or not user.is_active:
        return redirect('/login/Invalid_credentials')
    else: 
        checker.session_login(user)
        return redirect('/login/?next=' + nexturl)
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

