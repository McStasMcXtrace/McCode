#!/usr/bin/env python
'''
SocketClient which receives and unpickles a plot graph over TCP/IP.
'''
import logging
import argparse
import socket
import sys
import os
import pickle

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mclib.mcplotgraph import PlotGraphPrint

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_address = ('localhost', 10000)
    print >>sys.stderr, 'connecting to %s port %s' % server_address
    sock.connect(server_address)
    sock.settimeout(0.1)
    
    try:
        message = 'simfile: %s' % args.simfile
        print >>sys.stderr, 'sending "%s"' % message
        sock.sendall(message)
        
        text = ''
        try:
            while True:
                data = sock.recv(256)
                text = text + data
        except:
            print 'timeout reached'
        
        PlotGraphPrint(pickle.loads(text))
        
    finally:
        sock.close()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    #parser.add_argument('ip', nargs='*', help='prefix for file load requests from clients')
    #parser.add_argument('port', nargs=1, help='the port to serve the data at')
    parser.add_argument('simfile', nargs='?', help='simulation file or folder relative to server rootdir arg')
    #parser.add_argument('--localhost',  action='store_true', default=False, help='just server through localhost')
    args = parser.parse_args()

    main(args)
