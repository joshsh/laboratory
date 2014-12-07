package net.fortytwo.extendo.demos;

import com.illposed.osc.OSCBundle;
import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;
import com.illposed.osc.OSCPortOut;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.osc.OscControl;
import net.fortytwo.extendo.p2p.osc.OscSender;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * OSC controller for the Monomanual Typeatron
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronUdp extends TypeatronControlWrapper {

    private static final Logger logger = Logger.getLogger(TypeatronUdp.class.getName());

    private final String hostOut;
    private final int portIn, portOut;

    public TypeatronUdp(final String hostOut,
                        final int portIn,
                        final int portOut) throws OscControl.DeviceInitializationException {
        super();

        logger.info("connecting to Typeatron " + (null == hostOut ? "" : "at " + hostOut + " ") + "via UDP ports " + portIn + " (in) and " + portOut + " (out)");

        this.hostOut = hostOut;
        this.portIn = portIn;
        this.portOut = portOut;
    }

    public void run() throws SocketException, UnknownHostException {
        OSCPortIn pi = new OSCPortIn(portIn);
        pi.addListener("", new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                typeatron.getReceiver().receive(oscMessage);
            }
        });

        InetAddress outAddress = null == hostOut || hostOut.equals("localhost") || hostOut.equals("127.0.0.1")
                ? InetAddress.getLoopbackAddress() : InetAddress.getByName(hostOut);
        final OSCPortOut po = new OSCPortOut(outAddress, portOut);
        OscSender sender = new OscSender() {
            @Override
            public synchronized void send(OSCBundle bundle) {
                try {
                    po.send(bundle);
                } catch (IOException e) {
                    logger.log(Level.WARNING, "failed to send OSC bundle", e);
                }
            }

            @Override
            public void close() {
                // no-op
            }
        };
        typeatron.connect(sender);

        logger.info("listening for /exo messages");
        pi.startListening();
    }

    /*
     * Usage example:
     *   ./typeatron-serial.sh -d /dev/ttyUSB0 -r 115200
     */
    public static void main(final String[] args) throws Exception {
        try {
            Options options = new Options();

            Option hostOpt = new Option("h", "host", true, "host for outgoing OSC messages (default: 127.0.0.1)");
            hostOpt.setArgName("HOST");
            hostOpt.setRequired(false);
            options.addOption(hostOpt);

            Option portInOpt = new Option("i", "portIn", true, "port for incoming OSC messages (default: 42003)");
            portInOpt.setArgName("PORT_IN");
            portInOpt.setRequired(false);
            options.addOption(portInOpt);

            Option portOutOpt = new Option("o", "portOut", true, "port for outgoing OSC messages (default: 42002)");
            portOutOpt.setArgName("PORT_OUT");
            portOutOpt.setRequired(false);
            options.addOption(portOutOpt);

            Option confOpt = new Option("c", "conf", true, "Extendo configuration file");
            confOpt.setArgName("CONF");
            confOpt.setRequired(false);
            options.addOption(confOpt);

            CommandLineParser clp = new PosixParser();
            CommandLine cmd = null;

            try {
                cmd = clp.parse(options, args);
            } catch (ParseException e) {
                printUsage(options);
                System.exit(1);
            }

            String hostOut = cmd.getOptionValue(hostOpt.getOpt(), "127.0.0.1");
            int portIn = Integer.valueOf(cmd.getOptionValue(portInOpt.getOpt(), "42003"));
            int portOut = Integer.valueOf(cmd.getOptionValue(portOutOpt.getOpt(), "42002"));

            String conf = cmd.getOptionValue(confOpt.getOpt());
            if (null != conf) {
                Extendo.addConfiguration(new File(conf));
            }

            new TypeatronUdp(hostOut, portIn, portOut).run();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void printUsage(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("typeatron-osc", options);
    }
}
