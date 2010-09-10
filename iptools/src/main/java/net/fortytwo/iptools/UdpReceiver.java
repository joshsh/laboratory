package net.fortytwo.iptools;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;

/**
 * User: josh
 * Date: Sep 10, 2010
 * Time: 10:34:15 AM
 */
public class UdpReceiver {
    private static final int RECEIVER_BUFFER_SIZE = 65507;

    private final int port;
    private boolean stopped;

    public UdpReceiver(final int port) throws IOException {
        this.port = port;
    }

    private void receive() throws IOException {
        System.out.println("listening for UDP packets on port " + port);
        
        DatagramSocket dsocket = new DatagramSocket(port);
        byte[] buffer = new byte[RECEIVER_BUFFER_SIZE];
        DatagramPacket packet = new DatagramPacket(buffer, buffer.length);

        while (!stopped) {
            dsocket.receive(packet);

            System.out.println("received:");
            System.out.println("\taddress: " + packet.getAddress());
            System.out.println("\tsocket address: " + packet.getSocketAddress());
            System.out.println("\tport: " + packet.getPort());
            System.out.println("\tlength: " + packet.getLength());
            System.out.println("\toffset: " + packet.getOffset());
            System.out.println("\tdata: " + new String(packet.getData()));

            //String payload = new String(buffer, 0, packet.getLength());
            //System.out.println(packet.getAddress().getHostName() + ": "
            //        + payload);

            // Reset the length of the packet before reusing it.
            packet.setLength(buffer.length);
        }
    }

    public void start() throws IOException {
        stopped = false;
        receive();
    }

    public void stop() {
        stopped = true;
    }

    public static void main(final String[] args) {
        try {
            if (1 == args.length) {
                int port = Integer.getInteger(args[0]);
                UdpReceiver r = new UdpReceiver(port);
                r.start();
            } else {
                printUsage();
                System.exit(1);
            }
        } catch (Throwable t) {
            t.printStackTrace();
            System.exit(1);
        }
    }

    private static void printUsage() {
        System.out.println("Usage:  UdpReceiver [port]");
    }
}
