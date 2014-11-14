package net.fortytwo.extendo.demos;

import gnu.io.CommPortIdentifier;
import gnu.io.NoSuchPortException;
import gnu.io.PortInUseException;
import gnu.io.SerialPort;
import gnu.io.UnsupportedCommOperationException;
import net.fortytwo.extendo.p2p.osc.OscControl;
import net.fortytwo.extendo.p2p.osc.OscSender;
import net.fortytwo.extendo.p2p.osc.SlipOscSender;
import net.fortytwo.extendo.util.SlipInputStream;
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

/**
 * Serial controller for the Monomanual Typeatron
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronSerial extends TypeatronControlWrapper {

    private final String device;
    private final int rate;

    public TypeatronSerial(String device, int rate) throws OscControl.DeviceInitializationException {
        super();

        this.device = device;
        this.rate = rate;
    }

    public void run()
            throws NoSuchPortException, PortInUseException, UnsupportedCommOperationException,
            IOException, SlipInputStream.PacketHandlerException, OscControl.DeviceInitializationException {

        CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier(device);
        SerialPort serialPort = (SerialPort) portIdentifier.open("exohand-port", 0);
        serialPort.setSerialPortParams(rate, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

        talkToTypeatron(serialPort.getInputStream(), serialPort.getOutputStream());
    }

    protected void talkToTypeatron(final InputStream inputStream,
                                   final OutputStream outputStream) throws OscControl.DeviceInitializationException {



        OscSender sender = new SlipOscSender(outputStream);
        typeatron.connect(sender);

        try {
            logger.log(Level.INFO, "starting SLIP+OSC listener");

            SlipInputStream slipStream = new SlipInputStream(inputStream);
            slipStream.receive(new SlipInputStream.PacketHandler() {
                public void handle(byte[] packet, int length) throws Exception {
                    if (!typeatron.getReceiver().receive(packet, length)) {
                        logger.warning("no handler for packet");
                    }
                }
            });
        } catch (Throwable t) {
            logger.log(Level.SEVERE, "SLIP+OSC listener failed with error", t);
        }
    }

    /*
     * Usage example:
     *   ./typeatron-serial.sh -d /dev/ttyUSB0 -r 115200
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
