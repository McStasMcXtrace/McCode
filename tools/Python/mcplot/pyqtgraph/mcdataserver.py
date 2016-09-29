#!/usr/bin/env python
'''
Script for using McDataPickleServer to send mccode data over TCP/IP.
'''
import logging
import argparse
from mcdataservice import RCSimFile, RCList, McDataPickleServer

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    port = 10001
    if args.port:
        port = args.port
    
    server = McDataPickleServer(port)
    server.listen()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    #parser.add_argument('ip', nargs='*', help='prefix for file load requests from clients')
    parser.add_argument('port', nargs='?', help='the port to serve the data at')
    #parser.add_argument('--localhost',  action='store_true', default=False, help='just server through localhost')
    args = parser.parse_args()
    
    main(args)

