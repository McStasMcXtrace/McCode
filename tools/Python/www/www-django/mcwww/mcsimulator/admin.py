from mcsimulator.models import *
from django.contrib import admin

for model, modelA in (
    (Job,        JobAdmin),
    (Simulation, None),
    (SimRun,     SimRunAdmin),
    (Param,      ParamAdmin),
    (ParamValue, None)):
    admin.site.register(model, modelA)
