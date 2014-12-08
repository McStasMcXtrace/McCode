''' A collection of stand-alone utility functions '''

import base64
import os


def new_key():
    ''' Generate a new, secure 120-bit (15 bytes) key in urlsafe base64 '''
    # We use 96-bit because it fits the base64 encoding (no padding, short id)
    # and really, anything over 80-bit is *secure enought* for us.
    return base64.urlsafe_b64encode(os.urandom(15))


def new_id():
    ''' Returns a printable and URL-safe, unique ID '''
    return new_key()
