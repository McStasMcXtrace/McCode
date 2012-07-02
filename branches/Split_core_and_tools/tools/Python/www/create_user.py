import sys
from getpass import getpass

from app import db_session
from models import User

from werkzeug.security import generate_password_hash


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print 'usage: %s username [password]' % sys.argv[0]
        sys.exit(1)

    username = sys.argv[1]
    print 'Username:', username

    if User.query.filter_by(username=username).count() > 0:
        print 'Abort: User already exists!'
        sys.exit(1)

    if len(sys.argv) == 3:
        print 'Password taken from argument'
        password = sys.argv[2]
    else:
        password = getpass('Enter password: ')

    user = User(username=username, passhash=generate_password_hash(password))

    print 'Saving user.'
    db_session.add(user)
    db_session.commit()
