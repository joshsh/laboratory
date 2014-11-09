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
        self.receiver.addCallback('/exo/hand/bandpass',self.receive_bandpass)
        self.receiver.addCallback('/exo/hand/error',self.receive_error)
        self.receiver.addCallback('/exo/hand/gesture',self.receive_gesture)
        self.receiver.addCallback('/exo/hand/info',self.receive_info)
        self.receiver.addCallback('/exo/hand/motion',self.receive_motion)
        self.receiver.fallback = self.fallback

    # note: temporary, development pattern
    def receive_bandpass(self,message, address):
        msg_list = message.getValues()
        print("bandpass: ", msg_list)

    def receive_error(self,message, address):
        msg_list = message.getValues()
        print("error: ", msg_list)

    def receive_gesture(self,message, address):
        msg_list = message.getValues()
        print("gesture: ", msg_list)

    def receive_info(self,message, address):
        msg_list = message.getValues()
        print("info: ", msg_list)

    def receive_motion(self,message, address):
        msg_list = message.getValues()
        print("motion: ", msg_list)

    def fallback(self, message, address):
        """
        Fallback for any unhandled message
        """
        print("Fallback:")
        print("  Got %s from %s" % (message, address))

if __name__ == "__main__":
    app = UDPReceiverApplication(udp_port)
    reactor.run()
