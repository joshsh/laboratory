package net.fortytwo.extendo.demos;

import gnu.io.CommPortIdentifier;
import gnu.io.NoSuchPortException;
import gnu.io.PortInUseException;
import gnu.io.SerialPort;
import gnu.io.UnsupportedCommOperationException;
import net.fortytwo.smsn.monitron.EventHandler;
import net.fortytwo.smsn.monitron.MonitronService;
import net.fortytwo.smsn.monitron.data.BooleanData;
import net.fortytwo.smsn.monitron.data.Data;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.events.MonitronEvent;
import net.fortytwo.smsn.monitron.events.Observation;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MonitronLogger {
    private static final Logger LOGGER = Logger.getLogger(MonitronLogger.class.getName());

    private final File directory;
    private final String device;
    private final int rate;
    private final Map<String, PrintStream> logFiles = new HashMap<String, PrintStream>();

    public MonitronLogger(final File directory,
                          final String device,
                          final int rate) {
        this.directory = directory;
        this.device = device;
        this.rate = rate;
    }

    public void run() throws NoSuchPortException, PortInUseException, UnsupportedCommOperationException, IOException {
        EventHandler handler = new EventHandler() {
            public void handleEvent(MonitronEvent e) throws EventHandlingException {
                if (e instanceof Observation) {
                    try {
                        appendObservation((Observation) e);
                    } catch (FileNotFoundException e1) {
                        throw new EventHandlingException(e1);
                    }
                }
            }
        };

        CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier(device);
        SerialPort serialPort = (SerialPort) portIdentifier.open("monitron-port", 0);
        serialPort.setSerialPortParams(rate, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

        InputStream in = serialPort.getInputStream();
        try {
            MonitronService service = new MonitronService(in, handler);
            service.run();
        } finally {
            in.close();
        }
    }

    private void appendObservation(final Observation obs) throws FileNotFoundException {
        String sensor = obs.getSensor().getLocalName();
        PrintStream ps = logFiles.get(sensor);
        if (null == ps) {
            File file = new File(directory, sensor + ".log");
            OutputStream out = new FileOutputStream(file, true);
            ps = new PrintStream(out);
            logFiles.put(sensor, ps);
        }

        Data data = obs.getData();

        ps.print(data.getSampleIntervalBeginning());
        ps.print('\t');
        ps.print(data.getSampleIntervalEnd());
        ps.print('\t');
        ps.print(data.getTotalMeasurements());
        ps.print('\t');
        if (data instanceof BooleanData) {
            ps.print(((BooleanData) data).getResult());
        } else if (data instanceof GaussianData) {
            ps.print(((GaussianData) data).getMinValue());
            ps.print('\t');
            ps.print(((GaussianData) data).getMaxValue());
            ps.print('\t');
            ps.print(((GaussianData) data).getMean());
            ps.print('\t');
            ps.print(((GaussianData) data).getVariance());
        } else {
            LOGGER.warning("data is of unknown type (" + data.getClass() + "): " + data);
        }
        ps.print('\n');
    }

    /*
        Usage example:
            ./monitron-logger.sh -d /dev/ttyUSB0 -r 115200 -l /data/monitron
     */
    public static void main(final String[] args) throws Exception {
        try {
            Options options = new Options();

            Option dirOpt = new Option("l", "logdir", true, "directory in which to create sensor data logs");
            dirOpt.setArgName("DIR");
            options.addOption(dirOpt);

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

            String dir = cmd.getOptionValue(dirOpt.getOpt(), System.getProperty("user.dir"));
            String device = cmd.getOptionValue(deviceOpt.getOpt());
            int rate = Integer.valueOf(cmd.getOptionValue(rateOpt.getOpt()));

            new MonitronLogger(new File(dir), device, rate).run();

        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void printUsage(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("monitron-logger", options);
    }
}
