# Python script for SLIP (Serial Line Internet Protocol) <--> UDP communication
# By Joshua Shinavier, 2014
#     www.fortytwo.net
# Originally based on serial_to_udp.py by Alex Olwal, 2012 03 24
#     www.olwal.com

import serial
import sys
import time
import traceback

from socket import *
from threading import Thread
from array import *

slip_end = 0xc0
slip_esc = 0xdb
slip_esc_end = 0xdc
slip_esc_esc = 0xdd

baud_rate = 115200

# the buffer is much larger than any message should be.
# Otherwise, large messages from serial cause this program to hang and occupy the port
serial_udp_buffer_size = 15000

udp_serial_buffer_size = 1500

serial_in_buffer = bytearray(serial_udp_buffer_size)

millis = lambda: int(round(time.time() * 1000))

def send(msg, ip, port):
    socket(AF_INET,SOCK_DGRAM).sendto(msg, (ip, port))

if ( len(sys.argv) == 1 ):
        print("Serial SLIP <--> UDP utility | Joshua Shinavier, 2014, http://fortytwo.net")
        print("Syntax: " + sys.argv[0] + " serial_port udp_ip(= 127.0.0.1) udp_in_port(= 5000) udp_out_port(=5001)")
        print("Example: " + sys.argv[0] + " /dev/tty.usbserial-xxx 127.0.0.1 5000 5001")
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

print("Reading from serial port: " + serial_port)
print("Sending to " + udp_ip + ":" + str(udp_out_port))

udp_out_port = int(udp_out_port)

stopped = False

def receive():
    serial_out_buffer = array('c', ' '*udp_serial_buffer_size)
    in_sock = socket(AF_INET, SOCK_DGRAM)
    in_sock.bind(("", udp_in_port))
    while (not stopped):
        nbytes, address = in_sock.recvfrom_into(serial_out_buffer)
        if (printing):
            print("UDP(" + str(udp_in_port) + ")->serial(" + serial_port + "): " + str(nbytes) + " bytes")
        for i in range(0, nbytes):
            b = serial_out_buffer[i]
            ob = ord(b)
            if (slip_end == ob):
                sport.write(chr(slip_esc))
                sport.write(chr(slip_esc_end))
            elif (slip_esc == ob):
                sport.write(chr(slip_esc))
                sport.write(chr(slip_esc_esc))
            else:
                sport.write(b)
        sport.write(chr(slip_end))

thread = Thread(target = receive)
thread.start()

def skip_to_end():
    while (True):
        b = sport.read()
        if 0 == len(b):
            continue
        elif (slip_end == ord(b)):
            break

current_error = None
sport = None

def begin_error(name):
    global current_error
    if (current_error is not None and current_error != name):
        sys.stdout.write('\n')
    if (current_error is None or current_error != name):
        sys.stdout.write(str(millis()) + ": " + name)
    else:
        sys.stdout.write('.')
    sys.stdout.flush()
    current_error = name
    #print(traceback.format_exc())
    if sport is not None:
        sport.close()
    time.sleep(1)

# loop while threads are running.
while not stopped:
    try:
        sport = serial.Serial( serial_port, baud_rate, timeout=1 )
        if current_error is not None:
            sys.stdout.write('\n')
        print(str(millis()) + ": connected")
        current_error = None

        skip_to_end()

        i = 0
        while (not stopped):
            b = sport.read()
            if (0 == len(b)):
                continue
            ob = ord(b)
            if (slip_end == ob):
                # the check for i>0 allows for SLIP variants in which packets both begin and end with END
                if (i > 0):
                    st = "".join(map(chr, serial_in_buffer[0:i]))
                    if (printing):
                        print("serial(" + serial_port + ")->UDP(" + str(udp_out_port) + "): " + str(len(st)) + " bytes")
                    send(st, udp_ip, udp_out_port)
                    i = 0
            elif (slip_esc == ob):
                b = sport.read()
                if (0 == len(b)):
                    break
                ob = ord(b)
                if (slip_esc_end == ob):
                    serial_in_buffer[i] = slip_end
                    i = i + 1
                elif (slip_esc_esc == ob):
                    serial_in_buffer[i] = slip_esc
                    i = i + 1
                else:
                    #raise Exception("illegal escape sequence: found byte " + str(ob) + " after SLIP_ESC")
                    print("illegal escape sequence: found byte " + str(ob) + " after SLIP_ESC")
                    skip_to_end()
            else:
                serial_in_buffer[i] = b
                i = i + 1
    except serial.serialutil.SerialException:
        begin_error("serial error")
    except OSError:
        begin_error("OS error")
    except KeyboardInterrupt:
        print("closing...")
        if sport is not None:
            sport.close()
        stopped = True
        thread.join()
        print("done")
