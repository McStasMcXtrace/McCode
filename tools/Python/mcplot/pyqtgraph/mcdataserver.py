#!/usr/bin/env python
'''
SocketServer which sends a pickled plot graph via TCP/IP.
'''
import logging
import argparse
import socket
import pickle
from threading import Thread

class McDataPickleServer(object):
    ''' receives, unpickles and dispatches RemoteCommand objects '''
    def __init__(self, port):
        '''  '''
        self.port = port
        
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_name = 'localhost'
        server_address = (server_name, self.port)
        print 'starting up on %s port %s' % server_address
        self.sock.bind(server_address)
        self.sock.listen(1)
    
    def listen(self):
        ''' execution loop '''
        
        try:
            while True:
                print 'waiting for a connection...'
                connection, client_address = self.sock.accept()
                print 'client connected:', client_address
                text = ''
                
                # NOTE: right now we assume, that we can get it all in one chunk with a blocking socket
                data = connection.recv(1024)
                text = text + data
                
                # unpickle and dispatch
                request = pickle.loads(text)
                print 'received request: "%s"' % request
                
                t = Thread()
                rsend = ReplySender(connection)
                t.run = lambda: request.handle(rsend)
                t.start()
        finally:
            self.sock.close()

class ReplySender():
    def __init__(self, connection):
        self.connection = connection
    
    def send_reply(self, reply):
        ''' callback for dispatched handlers - sends a reply though the socket '''
        try:
            s = pickle.dumps(reply)
            print "sending data..."
            self.connection.sendall(s)
        finally:
            self.connection.close()

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    server = McDataPickleServer(port=int(args.port[0]))
    server.listen()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    #parser.add_argument('ip', nargs='*', help='prefix for file load requests from clients')
    parser.add_argument('port', nargs=1, help='the port to serve the data at')
    #parser.add_argument('--localhost',  action='store_true', default=False, help='just server through localhost')
    args = parser.parse_args()
    
    main(args)

