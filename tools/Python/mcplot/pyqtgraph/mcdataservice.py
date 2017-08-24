'''
A "pickle" server and client implementation incapsulating request and reply implementations, 
along with implementations suitable for sending mccode data graph and a simple string reply
combined with a server listdir() output.

This can be used to set up remote mccode data browsing with low requirements.
'''
import sys
import os
import socket
import pickle
from threading import Thread

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mccodelib.mcplotloader import McCodeDataLoader

'''
Protocol implementation.
'''
class RemoteCommand(object):
    ''' subclass and implement impl, which should return a RemoteReply instance '''
    def handle(self, reply_sender):
        ''' reply_sender is a ReplyPickleSender '''
        reply = self.impl()
        reply_sender.send_reply(reply)

class RemoteReply(object):
    ''' subclass to implement server replies, NOTE: pickle.loads does not invoke __init__() '''
    def __init__(self):
        pass

class RCSimFile(RemoteCommand):
    def __init__(self, simfile):
        self.simfile = simfile
    
    def impl(self):
        loader = McCodeDataLoader(simfile=self.simfile)
        loader.load()
        graph = loader.plot_graph
        
        reply = RRSimfile(graph)
        return reply
    
    def __str__(self):
        return 'loading simfile: %s' % self.simfile

class RRSimfile(RemoteReply):
    def __init__(self, plotgraph):
        self.plotgraph = plotgraph

class RCList(RemoteCommand):
    def impl(self):
        lst = os.listdir('.')
        text = '\n'.join(lst)
        
        reply = RRList(text)
        return reply

class RRList(RemoteReply):
    def __init__(self, text):
        self.text = text

'''
Client / server implementation.
'''
class McDataPickleClient(object):
    ''' receives, unpickles and dispatches RemoteCommand objects '''
    def __init__(self, port):

        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_address = ('localhost', port)
        print('connecting to %s port %s' % server_address)
        self.sock.connect(server_address)
        self.sock.settimeout(0.5)
    
    def ask(self, request):
        try:
            print('sending request: "%s"' % request)
            
            text = pickle.dumps(request)
            self.sock.sendall(text)
            
            text = ''
            while True:
                try:
                    data = self.sock.recv(1024)
                    if data == '':
                        break
                    else:
                        text = text + data
                except Exception as e:
                    print('timeout reached')
            
            self.reply = pickle.loads(text)
            
        finally:
            self.sock.close()

class McDataPickleServer(object):
    ''' receives, unpickles and dispatches RemoteCommand objects '''
    def __init__(self, port):
        '''  '''
        self.port = port
        
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_name = 'localhost'
        server_address = (server_name, self.port)
        print('starting up on %s port %s' % server_address)
        self.sock.bind(server_address)
        self.sock.listen(1)
    
    def listen(self):
        ''' execution loop '''
        
        try:
            while True:
                print('waiting for a connection...')
                connection, client_address = self.sock.accept()
                print('client connected:', client_address)
                text = ''
                
                # NOTE: right now we assume, that we can get it all in one chunk with a blocking socket
                data = connection.recv(1024)
                text = text + data
                
                # unpickle and dispatch
                request = pickle.loads(text)
                print('received request: "%s"' % request)
                
                t = Thread()
                rsend = ReplyPickleSender(connection)
                t.run = lambda: request.handle(rsend)
                t.start()
        finally:
            self.sock.close()

class ReplyPickleSender():
    def __init__(self, connection):
        self.connection = connection
    
    def send_reply(self, reply):
        ''' callback for dispatched handlers - sends a reply though the socket '''
        try:
            s = pickle.dumps(reply)
            print("sending data...")
            self.connection.sendall(s)
        finally:
            self.connection.close()

