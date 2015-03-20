''' A collection of stand-alone utility functions '''

from django.http import HttpResponse, HttpResponseNotAllowed, HttpResponseForbidden
from django.utils import crypto
import simplejson
from django.core.context_processors import csrf

from django.shortcuts import get_object_or_404, render_to_response, redirect

import base64
import os


def jsonify(**kwargs):
    return HttpResponse(simplejson.dumps(kwargs))


def new_key():
    ''' Generate a new, secure, urlsafe key '''
    # Just use Django's random string generator - likely *secure enough* for us
    return crypto.get_random_string()


def one_or_none(lst):
    for i in lst:
        return i
    return None


def fetch(model, **kwargs):
    return model.objects.filter(**kwargs)

def fetch1(model, **kwargs):
    return one_or_none(fetch(model, **kwargs))

def exists(model, **kwargs):
    return model.objects.filter(**kwargs).count() > 0

def get_or_404(model, **kwargs):
    print 'get_or_404', model, kwargs
    return get_object_or_404(model, **kwargs)


def methods(f, methods=[]):
    def inner(req, *args, **kwargs):
        if req.method not in methods:
            return HttpResponseForbidden(
                'Invalid method ' + str(req.method) + '. \
                Allowed methods are ' + ', '.join(methods) + '.')

        return f(req, *args, **kwargs)
    return inner


def only_safe(f):
    return methods(f, ('GET','HEAD'))

def only_unsafe(f):
    return methods(f, ('POST',))



def templated(template):
    template = template + '.html'
    def outer(f):
        def inner(req, *args, **kwargs):
            vals = f(req, *args, **kwargs)
            vals.update(csrf(req))
            return render_to_response(template, vals)
        return inner
    return outer


def skip(excludes, items):
    for i in items:
        if i not in excludes:
            yield i
