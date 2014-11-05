package net.fortytwo.extendo.demos;

import gnu.io.CommPortIdentifier;
import gnu.io.NoSuchPortException;
import gnu.io.PortInUseException;
import gnu.io.SerialPort;
import gnu.io.UnsupportedCommOperationException;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.osc.OSCDispatcher;
import net.fortytwo.extendo.p2p.osc.SlipOscControl;
import net.fortytwo.extendo.typeatron.TypeatronControl;
import net.fortytwo.extendo.typeatron.ripple.Environment;
import net.fortytwo.extendo.util.SlipInputStream;
import net.fortytwo.ripple.RippleException;
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
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Serial controller for the Monomanual Typeatron
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronSerial {

    private static final Logger logger = Logger.getLogger(TypeatronSerial.class.getName());

    private final String device;
    private final int rate;

    public TypeatronSerial(String device, int rate) {
        this.device = device;
        this.rate = rate;
    }

    public void run()
            throws NoSuchPortException, PortInUseException, UnsupportedCommOperationException,
            IOException, SlipInputStream.PacketHandlerException, SlipOscControl.DeviceInitializationException {

        CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier(device);
        SerialPort serialPort = (SerialPort) portIdentifier.open("exohand-port", 0);
        serialPort.setSerialPortParams(rate, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

        talkToTypeatron(serialPort.getInputStream(), serialPort.getOutputStream());
    }

    public void talkToTypeatron(final InputStream inputStream,
                                final OutputStream outputStream) throws SlipOscControl.DeviceInitializationException {

        final OSCDispatcher dispatcher = new OSCDispatcher();

        ExtendoAgent agent = null;

        Environment environment = new Environment() {
            @Override
            public void speak(String message) throws RippleException {
                System.out.println("SPEAK: " + message);
            }

            @Override
            public boolean verbose() {
                return false;
            }
        };
        TypeatronControl typeatron = new TypeatronControl(dispatcher, agent, environment);

        typeatron.connect(outputStream);

        try {
            logger.log(Level.INFO, "starting SLIP+OSC listener");

            SlipInputStream slipStream = new SlipInputStream(inputStream);
            slipStream.receive(new SlipInputStream.PacketHandler() {
                public void handle(byte[] packet, int length) throws Exception {
                    dispatcher.receive(packet, length);
                }
            });
        } catch (Throwable t) {
            logger.log(Level.SEVERE, "SLIP+OSC listener failed with error", t);
        }
    }

    /*
    Usage example:
        ./typeatron-serial.sh -d /dev/ttyUSB0 -r 115200
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

            new TypeatronSerial(device, rate).run();

        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void printUsage(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("typeatron-serial", options);
    }
}
