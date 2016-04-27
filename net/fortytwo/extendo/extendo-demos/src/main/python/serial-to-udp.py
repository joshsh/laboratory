#
# Python script for serial to UDP communication
# Alex Olwal, 2012 03 24
# www.olwal.com
# 

import serial
import sys

from socket import *

def send(msg, ip, port):
    socket(AF_INET,SOCK_DGRAM).sendto(msg, (ip, port))

baud_rate = 115200

if ( len(sys.argv) == 1 ):
        print "Serial-to-UDP utility | Alex Olwal, 2012, www.olwal.com"
        print "Syntax: " + sys.argv[0] + " serial_port udp_ip(= 127.0.0.1) udp_port(= 5000)"
        print "Example: " + sys.argv[0] + " COM20 127.0.0.1 5000"
        quit()

serial_port = sys.argv[1]

if ( len(sys.argv) >= 3 ):
        udp_ip = sys.argv[2]
else:
        udp_ip = "127.0.0.1"

if ( len(sys.argv) >= 4 ):
        udp_port = sys.argv[3]
else:
        udp_port = "5000"
		
if ( len(sys.argv) >= 5):
        printing = 1
else:
		printing = 0

print "Reading from serial port: " + serial_port
print "Sending to " + udp_ip + ":" + udp_port

udp_port = int(udp_port)

s = serial.Serial( serial_port, baud_rate, timeout=1 )

while (1):
        line = s.readline()

        if (line != ''):
			if (printing):
				print line[:-1]
            
			send( line, udp_ip, udp_port )

        else:
            if (printing):
                print "."

s.close()

