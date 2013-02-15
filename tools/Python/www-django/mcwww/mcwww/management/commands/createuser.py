from django.core.management.base import BaseCommand, make_option

import sys
from getpass import getpass

from django.contrib.auth.models import User


def main(args):
    if len(args) < 1:
        print 'usage: %s username [password]' % args[0]
        sys.exit(1)

    username = args[0]
    print 'Username:', username

    if User.objects.filter(username=username).count() > 0:
        print 'Abort: User already exists!'
        sys.exit(1)

    if len(args) == 2:
        print 'Password taken from argument'
        password = args[1]
    else:
        password = getpass('Enter password: ')

    user = User.objects.create_user(username, password=password)
    user.save()

    print 'User created.'

class Command(BaseCommand):
    username = 'username'
    password = 'password'

    help = "Whatever you want to print here"

    def handle(self, *args, **options):
        main(args)
