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
            OUT_PINGS = 100,
            IN_PINGS = 100,
            PINGS_PER_CHUNK = 10;


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
                sendPings(in, out);
                receivePings(in, out);
            } finally {
                out.close();
            }
        } finally {
            in.close();
        }
    }

    private void sendPings(final InputStream in,
                           final OutputStream out) throws IOException {

        int chunks = OUT_PINGS / PINGS_PER_CHUNK;
        double[] latency = new double[chunks];
        for (int i = 0; i < chunks; i++) {
            long startTime = System.currentTimeMillis();
            for (int j = 0; j < PINGS_PER_CHUNK; j++) {
                out.write('i');
                int b = in.read();
                if ('o' != b) {
                    throw new IllegalStateException();
                }
            }
            long endTime = System.currentTimeMillis();
            double avTime = (endTime - startTime) / ((double) PINGS_PER_CHUNK);
            latency[i] = avTime;
        }

        Statistics stat = new Statistics(latency);
        System.out.println("round-trip min/avg/max/stddev = "
                + stat.getMin() + "/" + stat.getMean() + "/" + stat.getMax() + "/" + stat.getStdDev() + " ms");
    }

    private void receivePings(final InputStream in,
                              final OutputStream out) {

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
