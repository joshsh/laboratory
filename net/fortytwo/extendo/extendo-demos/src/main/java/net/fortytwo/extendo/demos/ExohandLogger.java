package net.fortytwo.extendo.demos;

import com.illposed.osc.OSCMessage;
import com.illposed.osc.utility.OSCByteArrayToJavaConverter;
import gnu.io.CommPortIdentifier;
import gnu.io.NoSuchPortException;
import gnu.io.PortInUseException;
import gnu.io.SerialPort;
import gnu.io.UnsupportedCommOperationException;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExohandLogger {
    private static final Logger LOGGER = Logger.getLogger(ExohandLogger.class.getName());

    // a somewhat arbitrary limit on the size of OSC-carrying datagrams we expect
    private static final int MAX_DATAGRAM_SIZE = 1500;

    // character 192: the SLIP protocol's "frame end" byte
    private static final int SLIP_FRAME_END = 0xc0;

    private final String device;
    private final int rate;
    private final Map<String, PrintStream> logFiles = new HashMap<String, PrintStream>();

    private final OSCByteArrayToJavaConverter converter;

    public ExohandLogger(final String device,
                         final int rate) {
        this.device = device;
        this.rate = rate;

        converter = new OSCByteArrayToJavaConverter();
    }

    public void run() throws NoSuchPortException, PortInUseException, UnsupportedCommOperationException, IOException {
        CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier(device);
        SerialPort serialPort = (SerialPort) portIdentifier.open("exohand-port", 0);
        serialPort.setSerialPortParams(rate, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

        byte[] buffer = new byte[MAX_DATAGRAM_SIZE];
        InputStream in = serialPort.getInputStream();
        try {
            int b;
            int i = 0;
            while (-1 != (b = in.read())) {
                if (SLIP_FRAME_END == b) {
                    if (i > 0) {
                        OSCMessage m = (OSCMessage) converter.convert(buffer, i);
                        System.out.print(m.getAddress());
                        for (Object arg : m.getArguments()) {
                            System.out.print("\t" + arg);
                        }
                    }
                    i = 0;
                } else {
                    buffer[i++] = (byte) b;
                }
            }
        } finally {
            in.close();
        }
    }

    /*
        Usage example:
            ./exohand-logger.sh -d /dev/ttyUSB0 -r 115200 > /tmp/exohand.log
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

            new ExohandLogger(device, rate).run();

        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void printUsage(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("exohand-logger", options);
    }
}
