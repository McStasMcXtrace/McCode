#!/usr/bin/env python
'''
Script for using McDataPickleClient to request and receive mccode data over TCP/IP.
'''
import logging
import argparse
import sys
import os
from mcdataservice import RCSimFile, RCList, McDataPickleClient

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mccodelib.mcplotgraph import PlotGraphPrint

def main(args):
    logging.basicConfig(level=logging.INFO)

    port = 10001
    
    client = McDataPickleClient(port)
    
    if args.list:
        client.ask(RCList())
        print('')
        print("server listdir('.') output:\n\n%s\n" % client.reply.text)
    
    elif args.simfile:
        client.ask(RCSimFile(simfile=args.simfile))
        print('')
        PlotGraphPrint(client.reply.plotgraph)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    #parser.add_argument('ip', nargs='*', help='prefix for file load requests from clients')
    #parser.add_argument('port', nargs=1, help='the port to serve the data at')
    parser.add_argument('simfile', nargs='?', help='simfile for server data request')
    parser.add_argument('--list',  action='store_true', default=False, help='server ls request')
    #parser.add_argument('--localhost',  action='store_true', default=False, help='just server through localhost')
    args = parser.parse_args()

    main(args)
