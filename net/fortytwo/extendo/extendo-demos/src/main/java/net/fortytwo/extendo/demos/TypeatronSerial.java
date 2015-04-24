package net.fortytwo.extendo.demos;

import gnu.io.NoSuchPortException;
import gnu.io.PortInUseException;
import gnu.io.UnsupportedCommOperationException;
import net.fortytwo.extendo.p2p.osc.OscControl;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import java.io.IOException;

/**
 * Serial controller for the Monomanual Typeatron
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronSerial extends TypeatronControlWrapper {

    private final SerialHelper serialHelper;

    public TypeatronSerial(String device, int rate) throws OscControl.DeviceInitializationException {
        super();

        serialHelper = new SerialHelper(this.typeatron, device, rate);
    }

    public void run() throws UnsupportedCommOperationException, IOException, PortInUseException, NoSuchPortException,
            OscControl.DeviceInitializationException {
        serialHelper.run();
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
