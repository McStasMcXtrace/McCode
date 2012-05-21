''' A collection of utility functions for templating and caching '''


from functools import wraps
from flask import request, render_template


def skip(disallowed, items):
    ''' Skip disallowed elements '''
    for i in items:
        if i not in disallowed:
            yield i


def templated(template=None):
    ''' Attach a template to a response handler '''
    def decorator(handle):
        ''' Decorator around handler '''
        @wraps(handle)
        def decorated_function(*args, **kwargs):
            ''' Function that gets called in place of handler '''
            template_name = template
            if template_name is None:
                template_name = request.endpoint \
                    .replace('.', '/') + '.html'
            ctx = handle(*args, **kwargs)
            if ctx is None:
                ctx = {}
            elif not isinstance(ctx, dict):
                return ctx
            return render_template(template_name, **ctx)
        return decorated_function
    return decorator
