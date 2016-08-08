'''

'''
import sys
import os
import pickle

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mclib.mcplotgraph import PlotGraphPrint
from mclib.mcplotloader import McPlotDataLoader

class RemoteCommand(object):
    ''' subclass and implement impl, which should return a RemoteReply instance '''
    def handle(self, reply_sender):
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
        loader = McPlotDataLoader(simfile=self.simfile)
        loader.load()
        graph = loader.plot_graph
        
        reply = RRSimfile(graph)
        return reply
    
    def __str__(self):
        return 'loading simfile: %s' % self.simfile

class RRSimfile(RemoteReply):
    def __init__(self, plotgraph):
        self.plotgraph = plotgraph

class RCList(RemoteReply):
    def impl(self):
        lst = os.listdir()
        text = '\n'.join(lst)
        
        reply = RRList(text)
        return reply

class RRList(RemoteReply):
    def __init__(self, text):
        self.text = text

