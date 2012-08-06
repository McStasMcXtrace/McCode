import os
from util import new_key

_basedir = os.path.abspath(os.path.dirname(__file__))


# List of admin mails
ADMINS = frozenset(['jsbn@fysik.dtu.dk'])

# Display tracebacks in HTML when an error occurs
DEBUG = True



# Database connection
#
# See: http://packages.python.org/Flask-SQLAlchemy/config.html
# URI examples (SQLite, MySQL, PostgreSQL, Oracle):
#  sqlite:////absolute/path/to/foo.db
#  mysql://username:password@server/db
#  postgresql://scott:tiger@localhost/mydatabase
#  oracle://scott:tiger@127.0.0.1:1521/sidname

SQLALCHEMY_DATABASE_URI = 'sqlite:///' + os.path.join(_basedir, 'data/app.db')
DATABASE_CONNECT_OPTIONS = {}


# IMAGE_FORMAT is either 'gif' or 'png' - R-based plotter only supports png
IMAGE_FORMAT = 'png'

# Use MPI to utilise N processors (disabled when 0)
MPI_NP = 0


# Set URL path for static files (e.g. /static/jquery.js)
STATIC_PATH = '/static'


# Maximum number of neutrons allowed (ray samples / ncount)
MAX_RAY_SAMPLES = 10000000

# Maximum number of scan points allowed (npoints)
MAX_SCAN_POINTS = 1000


# Generate a key for signing cookies (resets when server is restartet)
SECRET_KEY = new_key()

# Enable protection against cross-site request forgery in WTF
# (not currently needed, util.py does this with nonce)
CSRF_ENABLED = True
CSRF_SESSION_KEY = new_key()



# Cache templates in-memory
CACHE_TYPE = 'simple'
CACHE_THRESHOLD = 5  # seconds
