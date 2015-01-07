#======================================================#
# Django micro server                                  #
# -------------------                                  #
# Template Author: Andrew Pendleton                    #
# ---------------------------------                    #
# - serve_centre.py supplies server functionality      #
# - single view supplying upload page                  #
# Edit to provide views for your app.                  #
# Still have to build models/forms separately.         #
# ------------------------                             #
# Editor: Mark Lewis                                   #
#======================================================#

#======================================#
# Server Setup                         #
# ------------                         #
# Needs to go first to ensure settings #
# are initialised correctly.           #
#======================================#
import serve_centre 
#---------------------------#
# app specific config setup #
#---------------------------#
config_opts = dict(LOGIN_URL = './login',
                   AUTHENTICATION_BACKENDS=('models.mcBackend'),
                   AUTH_USER_MODEL        =('models.mcUser'),
                   DATABASES              ={'default':{'ENGINE':   'django.db.backends.sqlite3',
                                                       'NAME':     './sqlite3_DB/mcUser.db',
                                                       'USER':     '',
                                                       'PASSWORD': '',
                                                       'HOST':     '',
                                                       'PORT':     '',
                                                       }
                                            },
                   )
#-------------------------------------------#               
serve_centre.configure(options=config_opts) # <------ do not do this in any other file.
#-------------------------------------------#            ^^^

#================#
# Import Section #
#================#
#----------------#
# server imports #
#----------------#
from django.shortcuts import render_to_response, render,redirect
from django.template import RequestContext, Template, Context
from django.http import HttpResponseRedirect, HttpResponse
from django.core.urlresolvers import reverse
#------------------------#
# django for app imports #
#------------------------#
from django.conf.urls import include
from django.contrib.auth.decorators import login_required
#-------------#
# app imports #
#-------------#
from common import *
from mcUser.models import *

#-----------------------------------------------------------------------------------------------#

#=========================================#
# views                                   #
# -----                                   #
# Try to keep this file short.            #
# Don't do anything too clever.           #
# Make new views file and import instead. #
#      (nb. import server_centre in them) #
#=========================================#
@serve_centre.route(r'^$')
def front(request):
    return render(request, 'index.html', {})

#--------------------------------------------#
# login_form                                 #
# -----------                                #
# Refers the template website 'login' to the #
# client                                     #
# GETs the dictionary to python vars from    #
# template page passed in the decorator      #
#--------------------------------------------#
@serve_centre.route(r'^login/$', {}, name='login')
@templated('login')
def login_form(req):
    return dict(next = req.GET.get('next','/'))

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
@serve_centre.route(r'^loginPOST/$')
def loginPOST(req):
    form = req.POST
    nexturl = form.get('next', '/')
    UID = form.get('uid', '')
    PW = form.get('password', '')
    '''
    PUT THIS AS THE USER OBJECT
    '''
    checker = mcBackend()
    user = checker.authenticate(UID, PW)
#
#    if user is None or not user.is_active:
#        return redirect('/login/?next=' + nexturl)
    # if login is correct then log the user into the django session
    # probably need to rewrite the login method too
    login(reg, user)
    return redirect(nexturl)
#---------------------------------#
# logout_user                     #
# -----------                     #
# If the user is logged in then   # 
# log them out                    #
# redirect to login page (the     #
# template argument of the called #
# function)                       #
#---------------------------------#
@serve_centre.route(r'^logout_user/$')
def logout_user(req):
    if req.user is not None:
        logout(req)                              #  <--- will probably have to rewrite this function too
    return redirect('login_form')


#------------------------#
# call to run the server #
#------------------------#
if __name__ == '__main__':
    serve_centre.run()
