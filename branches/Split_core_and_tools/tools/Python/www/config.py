import os
_basedir = os.path.abspath(os.path.dirname(__file__))

from random import SystemRandom
PRNG = SystemRandom()

import string
ALPHA = string.letters + string.digits


DEBUG = True

ADMINS = frozenset(['mxw968@alumni.ku.dk'])
SECRET_KEY = ''.join(PRNG.choice(ALPHA) for _ in xrange(32))


SQLALCHEMY_DATABASE_URI = 'sqlite:///' + os.path.join(_basedir, 'data/app.db')
DATABASE_CONNECT_OPTIONS = {}

THREADS_PER_PAGE = 8

CSRF_ENABLED = True
CSRF_SESSION_KEY = 'ZF9mckN4+uFOyXpNj5Xx1g'


CACHE_TYPE = 'simple'
CACHE_THRESHOLD = 5  # seconds


IMAGE_FORMAT = 'png' # gif/png - R-based plotter only supports png

