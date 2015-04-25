package net.fortytwo.extendo.demos;

import gnu.io.CommPortIdentifier;
import gnu.io.NoSuchPortException;
import gnu.io.PortInUseException;
import gnu.io.SerialPort;
import gnu.io.UnsupportedCommOperationException;
import net.fortytwo.extendo.p2p.osc.OscControl;
import net.fortytwo.extendo.p2p.osc.OscSender;
import net.fortytwo.extendo.p2p.osc.SlipOscSender;
import net.fortytwo.extendo.util.slip.SlipInputStream;
import net.fortytwo.extendo.util.slip.SlipOutputStream;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * An object to control an OSC component through a serial port
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class OscSerialAdapter {
    protected static final Logger logger = Logger.getLogger(OscSerialAdapter.class.getName());

    private final OscControl controller;
    private final String device;
    private final int rate;

    /**
     * Constructs a new adapter
     *
     * @param controller a controller for the remote OSC component
     * @param device the hardware address of the serial port
     * @param rate the data rate in bauds
     */
    public OscSerialAdapter(final OscControl controller,
                            final String device,
                            final int rate) {
        this.controller = controller;
        this.device = device;
        this.rate = rate;
    }

    /**
     * Listens for incoming OSC messages and sends outgoing OSC messages indefinitely
     */
    public void run() throws OscControl.DeviceInitializationException, IOException,
            UnsupportedCommOperationException, PortInUseException, NoSuchPortException {

        CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier(device);
        SerialPort serialPort = (SerialPort) portIdentifier.open("exohand-port", 0);
        serialPort.setSerialPortParams(rate, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

        InputStream inputStream = serialPort.getInputStream();
        OutputStream outputStream = serialPort.getOutputStream();

        // note: using the threaded version of SlipOutputStream, as invalid OSC bundles may otherwise result
        // when messages originate in multiple threads
        SlipOutputStream sos = new SlipOutputStream(outputStream, true);
        // TODO: it is not entirely understood why throttling messages is also necessary
        // With round-trip latency over RXTX at around 10ms on a particular system, it seems the throttling period
        // needs to exceed this.
        sos.setThrottlingPeriod(40);
        OscSender sender = new SlipOscSender(sos);
        controller.connect(sender);

        try {
            logger.log(Level.INFO, "starting SLIP+OSC listener");

            SlipInputStream slipStream = new SlipInputStream(inputStream);
            slipStream.receive(new SlipInputStream.PacketHandler() {
                public void handle(byte[] packet, int length) throws Exception {
                    if (!controller.getReceiver().receive(packet, length)) {
                        logger.warning("no handler for packet");
                    }
                }
            });
        } catch (Throwable t) {
            logger.log(Level.SEVERE, "SLIP+OSC listener failed with error", t);
        }
    }
}
