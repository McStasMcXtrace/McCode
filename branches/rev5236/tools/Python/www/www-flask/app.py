from flask import *
from flask.ext.sqlalchemy import SQLAlchemy
from sqlalchemy.orm import scoped_session, sessionmaker
from sqlalchemy.ext.declarative import declarative_base

# Setup application
import config
app = Flask(__name__, static_path=config.STATIC_PATH)
app.config.from_object('config')


# Setup database connection
db = SQLAlchemy(app)
SessionMaker = sessionmaker(bind=db.engine)
db_session = scoped_session(SessionMaker)

ModelBase = declarative_base()
ModelBase.query = db_session.query_property()
