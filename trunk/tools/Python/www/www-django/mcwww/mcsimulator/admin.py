# Mark Lewis 2014 - changes to admin site. Stage 2.
#                  ================================
#                  Finish SimRun and Job Admins
#                  Extend UserAdmin with some McSim specific thingies.

from mcsimulator.models import *
from django.forms.models import BaseInlineFormSet # For customisation of the form in the future, but right now it's a bit complicated
from django.contrib import admin

# Simulation Parameter Handling
# -----------------------------
#
# - Set the default add permission to False in ParamAdmin and ParamValueAdmin
#   (does not seem to be inherited by SimulationAdmin - MUST FIND WORKAROUND)
# 
# - Setting formating for Simulation and SimRun Admins

class ParamAdmin(admin.ModelAdmin):
    def has_add_permission(self, request, obj=None):
        return False

class ParamValueAdmin(admin.ModelAdmin):
    def has_add_permission(self, request, obj=None):
        return False

class TabbedParams(admin.TabularInline):     # May be nice to work out how to customise further.
    model = Param                            # Param = default parameter value simulations not on the stack.
    exclude = ('priority',)
    readonly_fields = ('name', 'unit')
    fields = ('name', 'msg', 'str_default', 'unit')
    actions = None
    extra = 0
    # Code to hide the 'add' and 'delete' functionality, Remember to call the super __init__!!
    def has_add_permission(self, request, obj=None):
        return False
    def has_delete_permission(self, request, obj=Param):
        return False
    def __init__(self, *args, **kwargs):
        super(TabbedParams, self).__init__(*args, **kwargs)
        self.list_display_links = (None,)

class StackedParamVals(admin.StackedInline):
    model = ParamValue
    extra = 0


# Simulation DB Handling
# ----------------------

class SimulationAdmin(admin.ModelAdmin):
    exclude = ('displayname', 'simgroup')
    readonly_fields =  ('name',)
    inlines = [TabbedParams]
    def delete(self, *args, **kwargs):     # want to write a signal to send telling server to move simulations to a sim_trash folder.
        super(SimulationAdmin, self).delete(*args, **kwargs)
        #non-DB related code here



# Running/Stacked Simultaion Handling - not working on this yet, still got to get the simulation page working. * = not urgent
# -----------------------------------
# 
# - JobAdmin Handles the non-running, on the stack simulations.
# - SimRunAdmin handles the running, on the stack simulations.
# - SimRunAdmin inherits (has a 1-to-1 relationship) with JobAdmin
#
# - Should include:
#         searching/filtering with status
#         searching by user name
#         ordering by something, maybe user/created posn on stack (if change job ordering) *
# 
# - Functionality:
#         Job Promotion/Demotion *
#         Job Deletion
#         Kill Run (server side for clean up)
#         Removing Job from stack (server side clean up)
#         Pause (server side clean up and rebuild) *


class JobAdmin(admin.ModelAdmin):
    readonly_fieldsets = [
        ('Job Data',          {'fields': ['ref','created']}),
        ]

class SimRunAdmin(JobAdmin):
    readonly_fieldsets = [
        ('user data',         {'fields' : ['user'], 'classes': ['collapse']}),
        ('SimRun Data',       {'fields' : ['created', 'status']}),
        ('RunParameters',     {'fields' : [''], 'classes': ['collapse']}),
        ]
    search_fields = ['user']
    list_filter   = ['status']



# Want to get a class here that inherits from UserAdmin and adds a little functionality, like filters by on/off-line etc.


for model, modelA in (
    (Job,        JobAdmin),
    (Simulation, SimulationAdmin),
    #(SimRun,     SimRunAdmin),
    #(Param,      ParamAdmin),
    #(ParamValue, ParamValueAdmin)
    ): # Param and ParamValue Admins may not be needed
    admin.site.register(model, modelA)
