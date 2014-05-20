# Mark Lewis 2014 - changes to admin site. Stage 1.
#                  =================================
#                  make std django admin forms for each
#                  model in mcsimulator.

from mcsimulator.models import *
from django.contrib import admin

# Explicit declaration of ordering of the model admin.

class JobAdmin(admin.ModelAdmin):
    readonly_fieldsets = [
        ('Job Data',          {'fields': ['ref','created']}),
        ]
    
    

class SimulationAdmin(admin.ModelAdmin):
    pass
    #put the fieldsets and ordering here.

class SimRunAdmin(admin.ModelAdmin):
    readonly_fieldsets = [
        ('user data',         {'fields': ['user'], 'classes': ['collapse']}),
        ('RunParameters',     {''' in here we need to put an access to the run p[arameters, can we call self.params here?'''  },
        ('SimRun Data',       {'fields': ['created', 'status']}),
        ]
    list_filter = ['status']

class ParamAdmin(admin.ModelAdmin):
    pass
    #put the fieldsets and ordering here.

class ParamValueAdmin(admin.ModelAdmin):
    pass
    #put the fieldsets and ordering here.

# Want to get a class here that inherits from UserAdmin and adds a little functionality, like filters by on/off-line etc.


for model, modelA in (
    (Job,        JobAdmin),
    (Simulation, SimulationAdmin),
    (SimRun,     SimRunAdmin),
    (Param,      ParamAdmin),
    (ParamValue, ParamValueAdmin)): # Param and ParamValue Admins may not be needed
    admin.site.register(model, modelA)
