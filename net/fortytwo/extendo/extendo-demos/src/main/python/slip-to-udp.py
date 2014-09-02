# Python script for SLIP (Serial Line Internet Protocol) <--> UDP communication
# By Joshua Shinavier, 2014-08-29
#     www.fortytwo.net
# Based on serial_to_udp.py by Alex Olwal, 2012 03 24
#     www.olwal.com

import serial
import sys

from socket import *
from threading import Thread
from array import *

baud_rate = 115200

serial_in_buffer = bytearray(1500)

def send(msg, ip, port):
    socket(AF_INET,SOCK_DGRAM).sendto(msg, (ip, port))

if ( len(sys.argv) == 1 ):
        print "SLIP<-->UDP utility | Joshua Shinavier, 2014, http://fortytwo.net"
        print "Syntax: " + sys.argv[0] + " serial_port udp_ip(= 127.0.0.1) udp_in_port(= 5000) udp_out_port(=5001)"
        print "Example: " + sys.argv[0] + " COM20 127.0.0.1 5000 5001"
        quit()

serial_port = sys.argv[1]

if ( len(sys.argv) >= 3 ):
        udp_ip = sys.argv[2]
else:
        udp_ip = "127.0.0.1"

if ( len(sys.argv) >= 5 ):
        udp_in_port = int(sys.argv[3])
        udp_out_port = int(sys.argv[4])
else:
        udp_in_port = 5000
        udp_out_port = 5001
		
if ( len(sys.argv) >= 6):
    printing = 1
else:
	printing = 0

print "Reading from serial port: " + serial_port
print "Sending to " + udp_ip + ":" + str(udp_out_port)

udp_out_port = int(udp_out_port)

sport = serial.Serial( serial_port, baud_rate, timeout=1 )

def receive():
    serial_out_buffer = array('c', ' '*1500)
    in_sock = socket(AF_INET, SOCK_DGRAM)
    in_sock.bind(("", udp_in_port))
    while (1):
        nbytes, address = in_sock.recvfrom_into(serial_out_buffer)
        if (printing):
            print "UDP(" + str(udp_in_port) + ")->serial(" + serial_port + "): " + str(nbytes) + " bytes"
        for i in range(0, nbytes):
            sport.write(serial_out_buffer[i])
        sport.write(chr(192))

thread = Thread(target = receive)
thread.start()

# Loop while threads are running.
try :
    # skip to the end of the current datagram
    while (1):
        b = sport.read()
        if (0 == len(b)):
            continue
        elif (192 == ord(b)):
            break

    i = 0
    while (True):
        b = sport.read()
        if (0 == len(b)):
            continue
        if (192 == ord(b)):
            if (i > 0):
                st = "".join(map(chr, serial_in_buffer[0:i]))
                if (printing):
                    print "serial(" + serial_port + ")->UDP(" + str(udp_out_port) + "): " + str(len(st)) + " bytes"
                send(st, udp_ip, udp_out_port)
                i = 0
        else:
            serial_in_buffer[i] = b
            i = i + 1

except KeyboardInterrupt :
    print "closing..."
    sport.close()
    thread.join()
    print "done"
