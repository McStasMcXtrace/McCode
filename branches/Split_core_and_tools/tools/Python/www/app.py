from flask import *
from flaskext.sqlalchemy import SQLAlchemy

# Setup application
app = Flask(__name__)
app.config.from_object('config')

# Setup database connection
db = SQLAlchemy(app)
