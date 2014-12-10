from mcsimulator.models import *
from django.forms import ModelForm
from django.forms.models import BaseInlineFormSet # For customisation of the form in the future, but right now it's a bit complicated
from django.contrib import admin

# Mark Lewis 2014 - changes to admin site. Stage 2.
#                   ===============================
#                  Finish SimRun and Job Admins
#                  Extend UserAdmin with some McSim specific thingies.
#                  
#                  Future Changes.
#                  ===============
#                  SimulationAdmin.delete()
#                  all the *'d tasks in Job and SimRun Admins
#
# - Permission changing code
#   ------------------------
'''
    def has_change_permission(self, request, obj=None):
        return False
    def has_add_permission(self, request, obj=None):
        return False
    def has_delete_permission(self, request, obj=None1):
        return False
    def __init__(self, *args, **kwargs):
        super(<CLASSNAME>, request, self).__init__(*args, **kwargs)
        self.list_display_links = (None,)
'''

# Trivial Param and ParamValue Admin declarations
# -----------------------------------------------

class ParamAdmin(admin.ModelAdmin):
    def has_add_permission(self, request, obj=None):
        return False

class ParamValueAdmin(admin.ModelAdmin):
    def has_add_permission(self, request, obj=None):
        return False

# Formatting classes 
# ------------------
#
# - Trying to make the admin site look nice.
# - Remove relevent permissions for change, ad and delete.

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

class TabbedSim(admin.TabularInline):
    model           = Simulation
    readonly_fields = ('name','displayname')
    actions         = None
    extra           = 0
    fieldset        = [
        ('Sim Name',     {'list_display' : ['name']}),
        ('Parameters',   {'inlines'      : ['TabbedParams']}),
        ]


# Simulation DB Handling
# ----------------------

class SimulationAdmin(admin.ModelAdmin):
    exclude = ('displayname', 'simgroup')
    readonly_fields =  ('name',)
    inlines = [TabbedParams]

    def delete(self, *args, **kwargs):     # want to write a signal to send telling server to move simulations to a sim_trash folder.
        super(SimulationAdmin, self).delete(*args, **kwargs)
        # have to remove from DBs:
        #   Params
        #   Simulation (done via super)
        # non-DB tasks:
        #   move instrument files to trash (do not physically delete)


# Running/Stacked Simultaion Handling. * = not urgent
# ------------------------------------
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
    exclude         = ('samples', 'npoints', 'seed')
    readonly_fields = ('ref', 'created', 'sim')
    search_field    = ['ref']
    list_display    = ('sim_details',)
#    inlines         = [TabbedSim] # causes: 'has no foreign key to' error - would like to find a way to sort this.
    def has_add_permission(self, request, obj=None):
        return False
    def __init__(self, *args, **kwargs):
        super(JobAdmin, self).__init__(*args, **kwargs)
#        self.list_display_links = (None,)


  
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
