#=================================================#
# Edits: Mark Lewis 2015                          #
# ----------------------                          #
# Actually Working on:                            #
# --------------------                            #
# - Param model removal                           #
# - Simulation, SimRun, and, Job Admins           #
#   - Hide some fields (choose these)             #
#   - Make composite admin and have interactions  #
#     between models hidden from the AdminUser    #
# Notes                                           #
# -----                                           #
# - Deletion of Simulations => File system access #
# - Adding simulations => compilation             #
# - STILL HAVE NOT INITIALISED MCSTAS GROUPS WHEN #
#   CREATING USER!                                #
#=================================================#
# django imports
from django.forms import ModelForm
from django.forms.models import BaseInlineFormSet # Form customisation
from django.contrib import admin
# app imports
from mcsimulator.models import *
#-----------------------------------------#
# Param and ParamValue Admin declarations # THESE SHOULD BE MADE REDUNDANT
#-----------------------------------------#
class ParamAdmin(admin.ModelAdmin):
    def has_add_permission(self, request, obj=None):
        return False
class ParamValueAdmin(admin.ModelAdmin):
    def has_add_permission(self, request, obj=None):
        return False

#---------------------------------------------#
# Please forgive me for the below code - Mark #
#---------------------------------------------#

#=======================#
# Admin Site Formatting #
#=======================#
#---------------------------------#
# TabbedParams                    #
# ------------                    #
# - This should be made redundant #
#---------------------------------#
class TabbedParams(admin.TabularInline):     
    model = Param                            
    exclude = ('priority',)
    readonly_fields = ('name', 'unit')
    fields = ('name', 'msg', 'str_default', 'unit')
    actions = None
    extra = 0
    # remove add permission
    def has_add_permission(self, request, obj=None):
        return False
    # remove del permission
    def has_delete_permission(self, request, obj=Param):
        return False
    def __init__(self, *args, **kwargs):
        super(TabbedParams, self).__init__(*args, **kwargs)
        self.list_display_links = (None,)
#----------------------------#
# TabbedJob                  #
# ---------                  #
# - admin has:               #
#   - no add permission      #
#   - search on Job.ref      #
# - ordered by creation date #
#----------------------------#
class TabbedJob(admin.TabularInline):
    model           = Job
    exclude         = ('samples', 'npoints')
    readonly_fields = ('ref', 'created')
    list_display    = ('ref', 'simdetails', 'created')
    search_field    = ['ref']
    actions         = None
    extra           = 0
    # remove add permission
    def has_add_permission(self, request, obj=None):
        return False
    def __init_(self, *args, ** kwargs):
        super(TabbedJob, self).__init__(*args, **kwargs)
        self.list_display_links = (None,)        
    class Meta:
        ordering = ['created']
#--------------------------------------------#
# TabbedSim                                  #
# ---------                                  #
# - NEEDS TO BE EXTENDED WHEN PARAM          #
#   IMPLEMENTATION CHANGED                   #
# - admin can add/del sims                   #
# - file system acces view should be written #
#--------------------------------------------#
class TabbedSim(admin.TabularInline):
    model           = Simulation
    readonly_fields = ('name','displayname')
    actions         = None
    extra           = 0
    fieldset        = [
        ('Sim Name',     {'list_display' : ['name']}),
        ('Parameters',   {'inlines'      : ['TabbedParams']}),
        ]
#--------------------------------------------#
# Simulation DB Handling                     #
# ----------------------                     #
# - NEEDS TO BE EXTENDED WHEN PARAM          #
#   IMPLEMENTATION CHANGED                   #
#--------------------------------------------#
class SimulationAdmin(admin.ModelAdmin):
    exclude = ('displayname', 'simgroup')
    readonly_fields =  ('name',)
    inlines = [TabbedParams]
    # File System access goes here
    def delete(self, *args, **kwargs):
        super(SimulationAdmin, self).delete(*args, **kwargs)
        # have to remove from DBs:
        #   Params                       - Changing this.
        #   Simulation (done via super)
        # non-DB tasks:
        #   move instrument files to trash (do not physically delete)
#====================================================================#
# Sim/Job/SimRunAdmins - Compile into one OverlordAdmin              #
# --------------------                                               #
# Running/Stacked Simultaion Handling. * = not urgent                #
# ------------------------------------                               #
# - Should include:                                                  #
#         searching/filtering with status                            #
#         searching by user name                                     #
#         ordering by something, maybe user/created posn             #
#                                on stack (if change job ordering) * #
# - Functionality:                                                   #
#         Job Promotion/Demotion *                                   #
#         Job Deletion                                               #
#         Kill Run (server side for clean up)                        #
#         Removing Job from stack (server side clean up)             #
#         Pause (server side clean up and rebuild) *                 #
#====================================================================#
class JobAdmin(admin.ModelAdmin):
    exclude         = ('samples', 'npoints', 'seed')
    readonly_fields = ('ref', 'created', 'sim')
    search_field    = ['ref']
    list_display    = ('sim_details',)
    def has_add_permission(self, request, obj=None):
        return False
    def __init__(self, *args, **kwargs):
        super(JobAdmin, self).__init__(*args, **kwargs)

class SimRunAdmin(admin.ModelAdmin):
    exclude         = ('str_params', 'str_result')
    readonly_fields = ('user', 'status', 'completed', 'ref', 'job', 'sim')
    list_display    = ('user', '__unicode__', 'created', 'status')
    search_field    = ['user', 'self.sim.name', 'self.job.ref']
    list_filter     = ('status',)
    class Meta:
        ordering = ['user', 'created']

# Want to get a class here that inherits from UserAdmin and adds a little functionality, like filters by on/off-line etc.

for model, modelA in (
    (Job,        JobAdmin),
    (Simulation, None),
    (SimRun,     SimRunAdmin),
    (Param,      None), #ParamAdmin),
    (ParamValue, None)):
    admin.site.register(model, modelA)
