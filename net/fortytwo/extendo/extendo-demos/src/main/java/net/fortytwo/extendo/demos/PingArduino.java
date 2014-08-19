package net.fortytwo.extendo.demos;

import gnu.io.CommPortIdentifier;
import gnu.io.NoSuchPortException;
import gnu.io.PortInUseException;
import gnu.io.SerialPort;
import gnu.io.UnsupportedCommOperationException;
import net.fortytwo.extendo.demos.util.Statistics;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PingArduino {
    private final String device;
    private final int rate;

    private static final int
            OUT_PINGS = 1000,
            PINGS_PER_CHUNK = 100;

    private static final byte
            PING = 'p',
            REPLY = 'r';

    public PingArduino(final String device,
                       final int rate) {
        this.device = device;
        this.rate = rate;
    }

    private void run() throws NoSuchPortException, PortInUseException, UnsupportedCommOperationException, IOException {
        CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier(device);
        SerialPort serialPort = (SerialPort) portIdentifier.open("arduino-port", 0);
        serialPort.setSerialPortParams(rate, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

        InputStream in = serialPort.getInputStream();
        try {
            OutputStream out = serialPort.getOutputStream();
            try {
                exhaustInput(in);
                sendPings(in, out);
            } finally {
                System.out.println("closing output");
                out.close();
            }
        } finally {
            System.out.println("closing input");
            in.close();
        }
    }

    private void exhaustInput(final InputStream in) throws IOException {
        while (in.available() > 0) {
            if (-1 == in.read()) break;
        }
    }

    private void sendPings(final InputStream in,
                           final OutputStream out) throws IOException {
        // exhaust initial abnormal replies to pings
        int count = 0;
        long startMillis = System.currentTimeMillis();
        while (true) {
            out.write(PING);
            if (in.available() > 0 && REPLY == in.read()) {
                break;
            }
            count++;
        }
        long endMillis = System.currentTimeMillis();
        if (count > 0) {
            System.out.println("skipped " + count + " abnormal ping replies in " + (endMillis - startMillis) + "ms");
        }

        startMillis = System.currentTimeMillis();
        int chunks = OUT_PINGS / PINGS_PER_CHUNK;
        double[] latency = new double[chunks];
        for (int i = 0; i < chunks; i++) {
            long startNanos = System.nanoTime();
            for (int j = 0; j < PINGS_PER_CHUNK; j++) {
                out.write(PING);
                while (0 == in.available());
                int b = in.read();
                if (REPLY != b) {
                    throw new IllegalStateException();
                }
            }
            long endNanos = System.nanoTime();
            double avTime = (endNanos - startNanos) / (((double) PINGS_PER_CHUNK) * 1000000);
            latency[i] = avTime;
        }
        endMillis = System.currentTimeMillis();

        Statistics stat = new Statistics(latency);
        System.out.format("round-trip min/avg/max/stddev = %.4f/%.4f/%.4f/%.4f ms ("
                + (chunks * PINGS_PER_CHUNK) + " pings)\n",
                stat.getMin(),
                stat.getMean(),
                stat.getMax(),
                stat.getStdDev());
        System.out.println("finished normal pings in " + (endMillis - startMillis) + "ms");
    }

    /*
    Usage example:
        ./ping-arduino.sh -d /dev/ttyUSB0 -r 115200
    */
    public static void main(final String[] args) throws Exception {
        try {
            Options options = new Options();

            Option deviceOpt = new Option("d", "device", true, "serial device from which to read (e.g. /dev/ttyUSB0)");
            deviceOpt.setArgName("DEVICE");
            deviceOpt.setRequired(true);
            options.addOption(deviceOpt);

            Option rateOpt = new Option("r", "rate", true, "Arduino's data rate in bits per second (e.g. 115200)");
            rateOpt.setArgName("RATE");
            rateOpt.setType(Integer.class);
            rateOpt.setRequired(true);
            options.addOption(rateOpt);

            CommandLineParser clp = new PosixParser();
            CommandLine cmd = null;

            try {
                cmd = clp.parse(options, args);
            } catch (ParseException e) {
                printUsage(options);
                System.exit(1);
            }

            String device = cmd.getOptionValue(deviceOpt.getOpt());
            int rate = Integer.valueOf(cmd.getOptionValue(rateOpt.getOpt()));

            new PingArduino(device, rate).run();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void printUsage(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("ping-arduino", options);
    }
}
