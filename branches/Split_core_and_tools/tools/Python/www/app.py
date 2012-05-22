from flask import *
from flask.ext.sqlalchemy import SQLAlchemy
from sqlalchemy.orm import scoped_session, sessionmaker
from sqlalchemy.ext.declarative import declarative_base

from flask.ext.cache import Cache

# Setup application
app = Flask(__name__)
app.config.from_object('config')


# Setup database connection
db = SQLAlchemy(app)
SessionMaker = sessionmaker(bind=db.engine)
db_session = scoped_session(SessionMaker)

ModelBase = declarative_base()
ModelBase.query = db_session.query_property()


# Setup cache
cache = Cache(app)
