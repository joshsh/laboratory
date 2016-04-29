package net.fortytwo.extendo.demos.eval;

import gnu.io.CommPortIdentifier;
import gnu.io.NoSuchPortException;
import gnu.io.PortInUseException;
import gnu.io.SerialPort;
import gnu.io.UnsupportedCommOperationException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BluetoothLatency {

    private byte[] buffer = new byte[100];
    private static final String
            CMD = "CMD\r\n",
            ERR = "ERR\r\n";

    private List<Long> samplePings(final String device, final int rate, int sampleSize)
            throws UnsupportedCommOperationException, NoSuchPortException, PortInUseException, IOException, InterruptedException {

        CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier(device);
        SerialPort serialPort = (SerialPort) portIdentifier.open("bluetooth-port", 0);
        serialPort.setSerialPortParams(rate, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

        InputStream inputStream = serialPort.getInputStream();
        try {
            OutputStream outputStream = serialPort.getOutputStream();
            try {
                outputStream.write("$$$".getBytes());
                outputStream.flush();
                Thread.sleep(500);
                expect(inputStream, CMD);

                List<Long> sample = new LinkedList<Long>();
                for (int i = 0; i < sampleSize; i++) {
                    sample.add(ping(inputStream, outputStream));
                }
                return sample;
            } finally {
                outputStream.close();
            }
        } finally {
            inputStream.close();
        }
    }

    private void expect(final InputStream in, final String expected) throws IOException {
        int n = expected.length();
        for (int i = 0; i < n; i++) {
            buffer[i] = (byte) in.read();
        }

        String prefix = new String(buffer).substring(0, n);
        if (!expected.equals(prefix)) {
            throw new IllegalStateException("expecting response of '" + expected + "', got '" + prefix + "'");
        }
    }

    private long ping(final InputStream in, final OutputStream out) throws IOException {
        while (in.available() > 0) {
            in.read();
        }

        //long before = System.nanoTime();
        long before = System.currentTimeMillis();
        out.write('g');
        out.write('\n');
        out.flush();

        expect(in, ERR);
        long after = System.currentTimeMillis();
        //long after = System.nanoTime();

        return after - before;
    }

    public static void main(final String[] args) throws Exception {
        int sampleSize = 1000;
        String device = "/dev/tty.RNBT-31C3-RNI-SPP";
        int rate = 9600;
        //int rate = 57600;
        //int rate = 115200;

        BluetoothLatency bl = new BluetoothLatency();
        List<Long> sample = bl.samplePings(device, rate, sampleSize);

        double mean = Stats.findMean(sample);
        double sd = Stats.findStandardDeviation(sample, mean);
        System.out.println("baud rate: " + rate);
        System.out.println("sample size: " + sampleSize);
        System.out.println("mean latency: " + mean);
        System.out.println("standard deviation: " + sd);

        System.out.println(Stats.toR(sample, "pings"));
    }
}
