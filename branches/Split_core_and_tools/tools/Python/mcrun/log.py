
import logging

# import and re-export log levels
from logging import DEBUG, INFO, WARNING, ERROR, CRITICAL, FATAL

HANDLER = None


class McRunException(Exception):
    pass


def setupLogger():
    """ Setup logging facilities """
    formatter = logging.Formatter('%(created).2f, %(levelname)8s: %(message)s')

    global HANDLER
    HANDLER = logging.StreamHandler()
    HANDLER.setLevel(logging.INFO)
    HANDLER.setFormatter(formatter)

    root = logging.getLogger()
    root.setLevel(logging.DEBUG)
    root.addHandler(HANDLER)


def setLogLevel(level):
    """ Set global level (e.g. debug) """
    HANDLER.setLevel(level)


def getLogger(name):
    """ Get sub logger (e.g. optimisation) """
    return logging.getLogger("mcstas." + name)
