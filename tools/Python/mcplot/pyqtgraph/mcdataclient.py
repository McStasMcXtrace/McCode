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
from mcdataservice import RCSimFile

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mclib.mcplotgraph import PlotGraphPrint

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_address = ('localhost', 10004)
    print 'connecting to %s port %s' % server_address
    sock.connect(server_address)
    
    sock.settimeout(0.5)
    
    try:
        request = RCSimFile(simfile=args.simfile)
        print 'sending request: "%s"' % request
        
        text = pickle.dumps(request)
        sock.sendall(text)
        
        text = ''
        while True:
            try:
                data = sock.recv(1024)
                if data == '':
                    break
                else:
                    text = text + data
            except Exception as e:
                print 'timeout reached'
        
        PlotGraphPrint(pickle.loads(text).plotgraph)
        
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
