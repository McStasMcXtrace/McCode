import os
_basedir = os.path.abspath(os.path.dirname(__file__))

DEBUG = True

ADMINS = frozenset(['mxw968@alumni.ku.dk'])
SECRET_KEY = 'RfQPKhaa1FP2OrwYHR8gNA'

SQLALCHEMY_DATABASE_URI = 'sqlite:///' + os.path.join(_basedir, 'data/app.db')
DATABASE_CONNECT_OPTIONS = {}

THREADS_PER_PAGE = 8

CSRF_ENABLED = True
CSRF_SESSION_KEY = 'ZF9mckN4+uFOyXpNj5Xx1g'


CACHE_TYPE = 'simple'
CACHE_THRESHOLD = 5  # seconds
