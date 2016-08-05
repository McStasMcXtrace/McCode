#!/usr/bin/env python
'''
SocketServer which sends a pickled plot graph via TCP/IP.
'''
import logging
import argparse
import socket
import sys
import os
import re
import pickle
from threading import Thread

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mclib.mcplotloader import McPlotDataLoader

#class McDataServerLoop(Thread):
#    ''' implement run(), press start() '''
#    def __init__(self, sock, rootdir):
#        self.sock = sock
#        self.rootdir = rootdir
#        Thread.__init__(self)

def run(sock):
    '''  '''
    while True:
        print >>sys.stderr, 'waiting for a connection'
        connection, client_address = sock.accept()
        try:
            print >>sys.stderr, 'client connected:', client_address
            while True:
                data = connection.recv(256)
                print >>sys.stderr, 'received "%s"' % data
                
                if data:
                    s = handle_request(data)
                    connection.sendall(s)
                else:
                    break
        finally:
            connection.close()

def handle_request(data):
    '''  '''
    m = re.search('simfile:\s+([\w\.]+)', data)
    if m:
        simfile = m.group(1)
        print 'simfile decoded: %s\n\n' % simfile
        
        loader = McPlotDataLoader(simfile=simfile)
        loader.load()
        graph = loader.plot_graph
        #PlotGraphPrint(graph)
        
        graph_str = pickle.dumps(graph, protocol=pickle.HIGHEST_PROTOCOL)
        return graph_str
    
    return 'None'

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    
    server_name = 'localhost'
    port = int(args.port[0])
    server_address = (server_name, port)
    
    print >>sys.stderr, 'starting up on %s port %s' % server_address
    
    sock.bind(server_address)
    sock.listen(1)
    rootdir = args.rootdir[0]
    
    run(sock)
    
    #try:
    #    thread = McDataServerLoop(sock, rootdir)
    #    thread.start()
    #    
    #    thread.join()
    #except:
    #    print 'exception received'

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    #parser.add_argument('ip', nargs='*', help='prefix for file load requests from clients')
    parser.add_argument('port', nargs=1, help='the port to serve the data at')
    parser.add_argument('rootdir', nargs='?', help='prefix for file load requests from clients')
    #parser.add_argument('--localhost',  action='store_true', default=False, help='just server through localhost')
    args = parser.parse_args()

    main(args)
