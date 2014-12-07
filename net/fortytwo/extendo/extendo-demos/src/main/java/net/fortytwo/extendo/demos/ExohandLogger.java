package net.fortytwo.extendo.demos;

import com.illposed.osc.OSCMessage;
import com.illposed.osc.utility.OSCByteArrayToJavaConverter;
import gnu.io.CommPortIdentifier;
import gnu.io.NoSuchPortException;
import gnu.io.PortInUseException;
import gnu.io.SerialPort;
import gnu.io.UnsupportedCommOperationException;
import net.fortytwo.extendo.util.slip.SlipInputStream;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import java.io.IOException;
import java.io.InputStream;
import java.util.Date;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExohandLogger {

    private final String device;
    private final int rate;

    private final OSCByteArrayToJavaConverter converter;

    public ExohandLogger(final String device,
                         final int rate) {
        this.device = device;
        this.rate = rate;

        converter = new OSCByteArrayToJavaConverter();
    }

    public void run()
            throws NoSuchPortException, PortInUseException, UnsupportedCommOperationException,
            IOException, SlipInputStream.PacketHandlerException {

        CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier(device);
        SerialPort serialPort = (SerialPort) portIdentifier.open("exohand-port", 0);
        serialPort.setSerialPortParams(rate, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

        InputStream in = serialPort.getInputStream();
        try {
            SlipInputStream slipStream = new SlipInputStream(in);
            slipStream.receive(new SlipInputStream.PacketHandler() {
                public void handle(byte[] buffer, int length) throws Exception {
                    OSCMessage m = (OSCMessage) converter.convert(buffer, length);
                    System.out.print(m.getAddress());
                    for (Object arg : m.getArguments()) {
                        System.out.print("\t");
                        if (arg instanceof Date) {
                            System.out.print(((Date) arg).getTime());
                        } else {
                            System.out.print(arg);
                        }
                    }
                    System.out.print("\n");
                }
            });
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
