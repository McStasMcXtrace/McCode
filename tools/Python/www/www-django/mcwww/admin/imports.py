#================================================#
# imports.py                                     #
# ----------                                     #
# Contains the necessary models and classes to   #
# run the admin site.                            #
# This is not an app - yet.                      #
# ------------------                             #
# Author: Mark Lewis                             #
#================================================#
#================#
# system imports #
#================#
import sys                  # NOT NECC?
import os                   # NOT NECC?
import time
from datetime import date
#=================#
# general imports # - check for necessity
#=================#
import re
#================#
# django imports #
#================#
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User, Group
from django.shortcuts import redirect
from django.http import HttpResponse
#=============#
# App imports #
#=============#
from mcUser import models
from mcsimulator.admin import Overlord # this is the overarching admin class
from common import *
#========#
# consts #
#========#
NONCE_NAME = 'csrfmiddlewaretoken'
