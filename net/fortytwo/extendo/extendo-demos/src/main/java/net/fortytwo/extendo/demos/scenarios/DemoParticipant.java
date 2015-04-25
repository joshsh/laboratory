package net.fortytwo.extendo.demos.scenarios;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.demos.SerialHelper;
import net.fortytwo.extendo.hand.ExtendoHandControl;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.osc.OscControl;
import net.fortytwo.extendo.p2p.osc.OscReceiver;
import net.fortytwo.extendo.p2p.osc.OscSender;
import net.fortytwo.extendo.p2p.osc.UdpOscSender;
import net.fortytwo.extendo.rdf.Activities;
import net.fortytwo.rdfagents.model.Dataset;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.io.IOUtils;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DemoParticipant {
    private static final Logger logger = Logger.getLogger(DemoParticipant.class.getName());

    // for now, queries will not expire, and will not need to be renewed
    private static final int QUERY_TTL = 0;

    // note: participant and owner are ideally the same person, but Arthur doesn't have his own PKB
    private static final String
            //AGENT_URI = "http://fortytwo.net/josh/things/CybU2QN"; // Arthur Dent
            DEFAULT_AGENT_URI = "http://fortytwo.net/josh/things/SBZFumn"; // Joshua Shinavier

    private final ExtendoAgent agent;
    private final ExtendoHandControl exoHand;

    private String listOfPeopleMet, listOfThingsReceived;

    private String loadQuery(final String name) throws IOException {
        String rawQuery = IOUtils.toString(DemoParticipant.class.getResourceAsStream(name));
        return rawQuery.replaceAll("<I>", "<" + agent.getAgentUri().stringValue() + ">");
    }

    private void shareAttention(final URI focus) throws IOException {
        logger.info("sharing attention on " + focus);

        Dataset d = Activities.datasetForAttentionActivity(System.currentTimeMillis(), agent.getAgentUri(), focus);
        agent.getQueryEngine().addStatements(Extendo.GESTURE_TTL, toArray(d));
    }

    public DemoParticipant(final ExtendoAgent agent,
                           final ExtendoHandControl exoHand,
                           final String listOfPeopleMet,
                           final String listOfThingsReceived)
            throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException,
            OscControl.DeviceInitializationException {

        this.agent = agent;
        this.exoHand = exoHand;

        this.listOfPeopleMet = listOfPeopleMet;
        this.listOfThingsReceived = listOfThingsReceived;

        // TODO: high-five query

        // TODO: shake, give, take, and high-five output to Max

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("point-to-known-person.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value actor = b.getValue("actor");
                        Value indicated = b.getValue("indicated");

                        if (indicated instanceof URI) {
                            try {
                                shareAttention((URI) indicated);
                            } catch (IOException e) {
                                logger.log(Level.WARNING, "failed to share attention", e);
                            }
                        } else {
                            logger.warning("value indicated is not a URI: " + indicated);
                        }

                        // ...then react with a local cue
                        exoHand.sendAlertMessage();

                        // log after reacting
                        logger.log(Level.INFO, agent.getAgentUri()
                                + "notified that " + actor + " pointed to known person " + indicated);
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("point-to-thing-with-topic-of-interest.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value actor = b.getValue("actor");
                        Value indicated = b.getValue("indicated");
                        Value topic = b.getValue("topic");

                        // share attention first...
                        if (indicated instanceof URI) {
                            try {
                                shareAttention((URI) indicated);
                            } catch (IOException e) {
                                logger.log(Level.WARNING, "failed to share attention", e);
                            }
                        } else {
                            logger.warning("value indicated is not a URI: " + indicated);
                        }

                        // ...then react with a local cue
                        exoHand.sendAlertMessage();

                        // log after reacting
                        logger.log(Level.INFO, agent.getAgentUri()
                                + "notified that " + actor + " pointed to thing " + indicated
                                + " with topic of interest " + topic);
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("my-takes.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value thing = b.getValue("thing");
                        Value giver = b.getValue("giver");

                        if (null != listOfThingsReceived) {
                            // TODO
                        }

                        // log after reacting
                        logger.log(Level.INFO, "" + agent.getAgentUri()
                                + " notified of item " + thing + " taken from " + giver);
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("my-handshakes.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value person = b.getValue("person");

                        System.out.println("HANDSHAKE...");

                        if (null != listOfPeopleMet) {
                            // TODO
                        }

                        // log after reacting
                        logger.log(Level.INFO, "" + agent.getAgentUri()
                                + " notified of handshake with " + person);
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("my-handshakes-common-acquaintance.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value person = b.getValue("person");
                        Value acquaintance = b.getValue("acquaintance");

                        int toneFrequency = 440;
                        int toneDurationMs = 50;
                        int color = 0xe0ff00;
                        int vibrationDurationMs = 300;

                        exoHand.sendMulticueMessage(toneFrequency, toneDurationMs, color, vibrationDurationMs);

                        // log after reacting
                        logger.log(Level.INFO, "" + agent.getAgentUri()
                                + " notified of common acquaintance " + acquaintance
                                + " via handshake with " + person);
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("my-handshakes-common-topic.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value person = b.getValue("person");
                        Value topic = b.getValue("topic");

                        int toneFrequency = 262;
                        int toneDurationMs = 50;
                        int color = 0x0000ff;
                        int vibrationDurationMs = 300;

                        exoHand.sendMulticueMessage(toneFrequency, toneDurationMs, color, vibrationDurationMs);

                        // log after reacting
                        logger.log(Level.INFO, "" + agent.getAgentUri()
                                + " notified of common topic " + topic
                                + " via handshake with " + person);
                    }
                });
    }

    private Statement[] toArray(Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
    }

    private static void printUsage(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("demo-participant", options);
    }

    public static void main(final String[] args) throws Exception {
        try {
            Options options = new Options();

            /*
            Option deviceOpt = new Option("d", "device", true, "serial device from which to read (e.g. /dev/ttyUSB0)");
            deviceOpt.setArgName("DEVICE");
            deviceOpt.setRequired(true);
            options.addOption(deviceOpt);

            Option rateOpt = new Option("r", "rate", true, "Arduino's data rate in bits per second (e.g. 115200)");
            rateOpt.setArgName("RATE");
            rateOpt.setType(Integer.class);
            rateOpt.setRequired(false);
            options.addOption(rateOpt);
            */

            Option exoHandHostOpt = new Option(
                    "h", "exoHandHost", true, "host name for Extend-o-Hand (e.g. 127.0.0.1)");
            exoHandHostOpt.setArgName("HOST");
            exoHandHostOpt.setRequired(true);
            options.addOption(exoHandHostOpt);

            Option exoHandPortOpt = new Option("p", "exoHandPort", true, "UDP port for Extend-o-Hand (e.g. 42003)");
            exoHandPortOpt.setArgName("PORT");
            exoHandPortOpt.setRequired(true);
            options.addOption(exoHandPortOpt);

            Option agentUriOpt = new Option("a", "agentUri", true, "URI of the participant");
            agentUriOpt.setArgName("URI");
            agentUriOpt.setRequired(false);
            options.addOption(agentUriOpt);

            Option peopleOpt = new Option("m", "peopleMet", true, "id of a list of people met");
            peopleOpt.setArgName("ID");
            peopleOpt.setRequired(false);
            options.addOption(peopleOpt);

            Option thingsOpt = new Option("r", "thingsReceived", true, "id of a list of things received");
            thingsOpt.setArgName("ID");
            thingsOpt.setRequired(false);
            options.addOption(thingsOpt);

            CommandLineParser clp = new PosixParser();
            CommandLine cmd = null;

            try {
                cmd = clp.parse(options, args);
            } catch (ParseException e) {
                printUsage(options);
                System.exit(1);
            }

            //String device = cmd.getOptionValue(deviceOpt.getOpt());
            //int rate = Integer.valueOf(cmd.getOptionValue(rateOpt.getOpt(), "115200"));
            String exoHandHost = cmd.getOptionValue(exoHandHostOpt.getOpt());
            int exoHandPort = Integer.valueOf(cmd.getOptionValue(exoHandPortOpt.getOpt()));
            String agentUri = cmd.getOptionValue(agentUriOpt.getOpt(), DEFAULT_AGENT_URI);
            String peopleMetId = cmd.getOptionValue(peopleOpt.getOpt());
            String thingsReceivedId = cmd.getOptionValue(thingsOpt.getOpt());

            // note: the Extend-o-Hand sends messages directly to the Gestural Server for the sake of
            // simplicity and low latency, but we send messages from here to Extend-o-Hand.
            OscReceiver receiver = new OscReceiver();
            ExtendoAgent agent = new ExtendoAgent(agentUri, true);
            ExtendoHandControl exoHand = new ExtendoHandControl(receiver, agent);
            OscSender sender = new UdpOscSender(exoHandHost, exoHandPort);
            exoHand.connect(sender);

            exoHand.sendMulticueMessage(440, 100, 0xff00ff, 500);

            // this simply adds queries
            DemoParticipant p = new DemoParticipant(agent, exoHand, peopleMetId, thingsReceivedId);

            //SerialHelper serialHelper = new SerialHelper(exoHand, device, rate);
            //serialHelper.run();

            // wait until killed
            Object lock = "";
            synchronized (lock) {
                lock.wait();
            }
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
