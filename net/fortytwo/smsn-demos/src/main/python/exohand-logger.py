#!/usr/bin/env python
"""
UDP txosc receiver and logger for use with Extend-o-Hand

Copyright 2014 by Joshua Shinavier
See: http://fortytwo.net/projects/extendo
"""

import sys

from twisted.internet import reactor
from txosc import dispatch
from txosc import async

if ( len(sys.argv) > 2 ):
    print("Extend-o-Hand logger | Joshua Shinavier, 2014, http://fortytwo.net")
    print("Syntax: " + sys.argv[0] + " udp_port(= 42003)")
    quit()

udp_port = int(sys.argv[1]) if (len(sys.argv) == 2) else 42003

class UDPReceiverApplication():

    def __init__(self, port):
        self.port = port
        self.receiver = dispatch.Receiver()
        self._server_port = reactor.listenUDP(self.port, async.DatagramServerProtocol(self.receiver))
        print("Listening on osc.udp://localhost:%s" % (self.port))
        #self.receiver.addCallback('/some/specific/address',self.some_callback)
        self.receiver.fallback = self.fallback

    def fallback(self, message, address):
        msg_list = message.getValues()
        print(address, msg_list)

if __name__ == "__main__":
    app = UDPReceiverApplication(udp_port)
    reactor.run()
