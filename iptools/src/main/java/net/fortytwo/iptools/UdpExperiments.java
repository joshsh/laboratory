package net.fortytwo.iptools;

import java.net.InetAddress;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.io.IOException;

/**
 * User: josh
* Date: Sep 10, 2010
* Time: 11:06:08 AM
*/
class UdpExperiments {
    private final InetAddress receiverAddress;
    private final int receiverPort;
    private final DatagramSocket socket;

    public UdpExperiments(final InetAddress receiverAddress,
                     final int receiverPort) throws SocketException {
        this.receiverAddress = receiverAddress;
        this.receiverPort = receiverPort;
        this.socket = new DatagramSocket();
    }

    public void start() throws IOException {
        byte[] bytes = "Hello world!".getBytes();

        DatagramPacket packet = new DatagramPacket(bytes, bytes.length, receiverAddress, receiverPort);
        socket.send(packet);
    }
       
    public static void main(final String[] args) {
        int receiverPort = 8006;

        try {
            final UdpReceiver b = new UdpReceiver(receiverPort);
            new Thread(new Runnable(){
                public void run() {
                    try {
                        b.start();
                    } catch (IOException e) {
                        e.printStackTrace(System.err);
                    }
                }
            }).start();

            UdpExperiments t = new UdpExperiments(InetAddress.getLocalHost(), receiverPort);
            t.start();
            b.stop();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
