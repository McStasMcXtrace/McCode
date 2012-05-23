''' A collection of utility functions for templating and caching '''

from functools import wraps
import uuid

from app import app
from flask import request, make_response, render_template


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
            ''' Gets called in place of handler '''
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


def get_session():
    ''' Return current session '''
    return app.open_session(request)


def get_nonce():
    ''' Get the nonce registered with session '''
    return get_session().get('nonce', None)


def with_nonce():
    ''' Add a random nonce for forms etc. in a secure cookie '''
    def decorator(handle):
        ''' Decorator around handler '''
        @wraps(handle)
        def decorated_function(*args, **kwargs):
            ''' Gets called in place of handler '''
            session = get_session()
            if not 'nonce' in session:
                session['nonce'] = str(uuid.uuid4())
            resp = make_response(handle(*args, **kwargs))
            app.save_session(session, resp)
            return resp
        return decorated_function
    return decorator


def check_nonce():
    ''' Make sure nonce from session matches form '''
    def decorator(handle):
        ''' Decorator around handler '''
        @wraps(handle)
        def decorated_function(*args, **kwargs):
            ''' Gets called in place of handler '''
            session = get_session()
            if 'nonce' not in session:
                return 'Denied', 403
            if session['nonce'] != request.form.get('__nonce', None):
                return 'Denied', 403
            return handle(*args, **kwargs)
        return decorated_function
    return decorator
