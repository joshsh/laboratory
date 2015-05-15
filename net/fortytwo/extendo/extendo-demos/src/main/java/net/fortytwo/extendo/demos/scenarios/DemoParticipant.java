package net.fortytwo.extendo.demos.scenarios;

import edu.rpi.twc.rdfstream4j.BindingSetHandler;
import edu.rpi.twc.rdfstream4j.QueryEngine;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.NoteQueries;
import net.fortytwo.extendo.demos.TypeatronUdp;
import net.fortytwo.extendo.hand.ExtendoHandControl;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.osc.OscControl;
import net.fortytwo.extendo.p2p.osc.OscReceiver;
import net.fortytwo.extendo.p2p.osc.OscSender;
import net.fortytwo.extendo.p2p.osc.UdpOscSender;
import net.fortytwo.extendo.rdf.Activities;
import net.fortytwo.extendo.typeatron.ripple.ExtendoBrainClient;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.ripple.StringUtils;
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
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Collection;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DemoParticipant {
    private static final Logger logger = Logger.getLogger(DemoParticipant.class.getName());

    // for now, queries will not expire, and will not need to be renewed
    private static final int QUERY_TTL = 0;

    // multicues should last no longer than the throttling of messages to ExoHand
    private static final int MULTICUE_LENGTH_MS = 50;

    // note: participant and owner are ideally the same person, but Arthur doesn't have his own PKB
    private static final String
            //AGENT_URI = "http://fortytwo.net/josh/things/CybU2QN"; // Arthur Dent
            DEFAULT_AGENT_URI = "http://fortytwo.net/josh/things/SBZFumn"; // Joshua Shinavier

    private final ExtendoAgent agent;
    private final ExtendoHandControl exoHand;

    private final ExtendoBrainClient exoBrainClient;

    private final OscSender notificationSender;

    private String listOfPeopleMet, listOfThingsReceived;

    private final Runtime runtime = Runtime.getRuntime();

    private final BlockingQueue<String> messagesToSpeak = new LinkedBlockingQueue<String>(10);

    private final Filter defaultFilter = new Filter();

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
            OscControl.DeviceInitializationException, ExtendoBrainClient.ExtendoBrainClientException {

        this.agent = agent;
        this.exoHand = exoHand;

        this.listOfPeopleMet = listOfPeopleMet;
        this.listOfThingsReceived = listOfThingsReceived;

        exoBrainClient = new ExtendoBrainClient();

        // TODO: host and port are temporary; they should be configurable
        try {
            notificationSender = new UdpOscSender("localhost", 42003);
        } catch (UnknownHostException e) {
            throw new IllegalStateException();
        } catch (SocketException e) {
            throw new IllegalStateException();
        }

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("point-to-known-person.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value actor = b.getValue("actor");
                        Value indicated = b.getValue("indicated");
                        Value indicatedName = b.getValue("indicatedName");

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

                        addNotification("you know: " + indicatedName.stringValue());

                        // log after reacting
                        logger.log(Level.INFO, agent.getAgentUri()
                                + "notified that " + actor + " pointed to known person "
                                + indicated + " (" + indicatedName + ")");
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("point-to-my-group.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value actor = b.getValue("actor");
                        Value indicated = b.getValue("indicated");
                        Value indicatedName = b.getValue("indicatedName");

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

                        addNotification("you are a member of: " + indicatedName.stringValue());

                        // log after reacting
                        logger.log(Level.INFO, agent.getAgentUri()
                                + "notified that " + actor + " pointed to your group "
                                + indicated + " (" + indicatedName + ")");
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("point-to-thing-with-topic-of-interest.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value actor = b.getValue("actor");
                        Value indicated = b.getValue("indicated");
                        Value topic = b.getValue("topic");
                        Value topicLabel = b.getValue("topicLabel");

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

                        addNotification("this is related to " + topicLabel.stringValue());

                        // log after reacting
                        logger.log(Level.INFO, agent.getAgentUri()
                                + "notified that " + actor + " pointed to thing " + indicated
                                + " with topic of interest " + topic + " (" + topicLabel + ")");
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("my-takes.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value thing = b.getValue("thing");
                        Value giver = b.getValue("giver");
                        Value giverName = b.getValue("giverName");

                        if (thing instanceof URI) {
                            addToThingsReceived((URI) thing);
                        }

                        if (giver instanceof URI) {
                            addToPeopleMet((URI) giver);
                        }

                        addNotification("handoff from " + giverName.stringValue());

                        // log after reacting
                        logger.log(Level.INFO, "" + agent.getAgentUri()
                                + " notified of item " + thing + " taken from " + giver + " (" + giverName + ")");
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("my-handshakes.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value person = b.getValue("person");

                        if (person instanceof URI) {
                            addToPeopleMet((URI) person);
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
                        Value acquaintanceName = b.getValue("acquaintanceName");

                        int toneFrequency = 440;
                        int toneDurationMs = MULTICUE_LENGTH_MS;
                        int color = 0xe0ff00;
                        int vibrationDurationMs = 300;

                        exoHand.sendMulticueMessage(toneFrequency, toneDurationMs, color, vibrationDurationMs);

                        addNotification("common acquaintance: " + acquaintanceName.stringValue());

                        // log after reacting
                        logger.log(Level.INFO, "" + agent.getAgentUri()
                                + " notified of common acquaintance " + acquaintance + " (" + acquaintanceName + ")"
                                + " via handshake with " + person);
                    }
                });

        agent.getQueryEngine().addQuery(
                QUERY_TTL, loadQuery("my-handshakes-common-topic.rq"), new BindingSetHandler() {
                    @Override
                    public void handle(BindingSet b) {
                        Value person = b.getValue("person");
                        Value topic = b.getValue("topic");
                        Value topicLabel = b.getValue("topicLabel");

                        int toneFrequency = 262;
                        int toneDurationMs = MULTICUE_LENGTH_MS;
                        int color = 0x0000ff;
                        int vibrationDurationMs = 300;

                        exoHand.sendMulticueMessage(toneFrequency, toneDurationMs, color, vibrationDurationMs);

                        addNotification("common topic: " + topicLabel.stringValue());

                        // log after reacting
                        logger.log(Level.INFO, "" + agent.getAgentUri()
                                + " notified of common topic " + topic + " (" + topicLabel + ")"
                                + " via handshake with " + person);
                    }
                });

        logger.info("finished adding queries");
    }

    public void run() throws InterruptedException {
        startSpeakerThread();

        // wait until killed
        Object lock = "";
        synchronized (lock) {
            lock.wait();
        }
    }

    private void addNotification(final String message) {
        if (!messagesToSpeak.contains(message)) {
            messagesToSpeak.offer(message);
        }
    }

    private void speakNotification(final String message) {
        Process p = null;
        try {
            String command = "say \"" + StringUtils.escapeString(message) + "\"";
            p = runtime.exec(command);
        } catch (IOException e) {
            logger.log(Level.WARNING, "'say' command failed", e);
        }
        if (null != p) {
            int exitCode = 0;
            try {
                exitCode = p.waitFor();
            } catch (InterruptedException e) {
                logger.log(Level.SEVERE, "interrupted while waiting for 'say' command", e);
            }
            if (0 != exitCode) {
                logger.warning("'say' command failed with code " + exitCode);
            }
        }
    }

    private Statement[] toArray(Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
    }

    private void startSpeakerThread() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    while (true) {
                        String message = messagesToSpeak.take();
                        speakNotification(message);
                    }
                } catch (Throwable t) {
                    logger.log(Level.SEVERE, "speaker thread died with error", t);
                }
            }
        }).start();
    }

    private void addToPeopleMet(final URI personUri) {
        if (null == this.listOfPeopleMet) {
            return;
        }

        String s = personUri.stringValue();
        int i = s.lastIndexOf('/');
        if (i <= 0) {
            return;
        }
        String id = s.substring(i+1);

        prepend(listOfPeopleMet, id, defaultFilter);
    }

    private void addToThingsReceived(final URI thingReceived) {
        if (null == this.listOfThingsReceived) {
            return;
        }

        String s = thingReceived.stringValue();
        int i = s.lastIndexOf('/');
        if (i <= 0) {
            return;
        }
        String id = s.substring(i+1);

        prepend(listOfThingsReceived, id, defaultFilter);
    }

    private void prepend(final String listId,
                         final String itemId,
                         final Filter filter) {
        Note item = new Note();
        item.setId(itemId);

        Note list = new Note();
        list.setId(listId);
        list.addChild(item);

        try {
            exoBrainClient.update(list, 1, filter, NoteQueries.forwardAddOnlyViewStyle);
        } catch (ExtendoBrainClient.ExtendoBrainClientException e) {
            logger.log(Level.SEVERE, "error while adding to list", e);
        }
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
                    "h", "host", true, "host name for Extend-o-Hand and Typeatron (e.g. 127.0.0.1)");
            exoHandHostOpt.setArgName("HOST");
            exoHandHostOpt.setRequired(true);
            options.addOption(exoHandHostOpt);

            Option exoHandPortOpt = new Option("p", "exoHandPort", true, "UDP port for Extend-o-Hand (e.g. 42003)");
            exoHandPortOpt.setArgName("PORT");
            exoHandPortOpt.setRequired(true);
            options.addOption(exoHandPortOpt);

            Option ttPortsOpt = new Option("t", "ttPorts", true, "in/out UDP ports for Typeatron (e.g. 42102,42103)");
            ttPortsOpt.setArgName("PORT,PORT");
            ttPortsOpt.setRequired(false);
            options.addOption(ttPortsOpt);

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
            String host = cmd.getOptionValue(exoHandHostOpt.getOpt());
            int exoHandPort = Integer.valueOf(cmd.getOptionValue(exoHandPortOpt.getOpt()));
            String agentUri = cmd.getOptionValue(agentUriOpt.getOpt(), DEFAULT_AGENT_URI);
            String peopleMetId = cmd.getOptionValue(peopleOpt.getOpt());
            String thingsReceivedId = cmd.getOptionValue(thingsOpt.getOpt());

            // for the Typeatron
            // TODO: let either or both Typeatron, ExoHand read the agent URI from a property
            Extendo.getConfiguration().setProperty(Extendo.P2P_AGENT_URI, agentUri);

            ExtendoAgent agent = null;
            //*
            String ttPorts = cmd.getOptionValue(ttPortsOpt.getOpt());
            if (null != ttPorts) {
                int i = ttPorts.indexOf(",");
                if (i > 0) {
                    int portIn = Integer.valueOf(ttPorts.substring(0, i));
                    int portOut = Integer.valueOf(ttPorts.substring(i+1));
                    final TypeatronUdp ttControl = new TypeatronUdp(host, portIn, portOut);
                    agent = ttControl.getTypeatron().getAgent();

                    new Thread(new Runnable() {
                        @Override
                        public void run() {
                            try {
                                ttControl.run();
                            } catch (Throwable t) {
                                logger.log(Level.SEVERE, "Typeatron thread died with error", t);
                            }
                        }
                    }).start();
                }
            }//*/

            // note: the Extend-o-Hand sends messages directly to the Gestural Server for the sake of
            // simplicity and low latency, but we send messages from here to Extend-o-Hand.
            OscReceiver receiver = new OscReceiver();
            if (null == agent) {
                agent = new ExtendoAgent(agentUri, true);
            }
            ExtendoHandControl exoHand = new ExtendoHandControl(receiver, agent);
            exoHand.setThrottlingPeriod(200);
            exoHand.throttleAsynchronously(5);
            OscSender sender = new UdpOscSender(host, exoHandPort);
            exoHand.connect(sender);

            exoHand.sendMulticueMessage(440, MULTICUE_LENGTH_MS, 0xff00ff, 500);

            // this simply adds queries
            DemoParticipant p = new DemoParticipant(agent, exoHand, peopleMetId, thingsReceivedId);
            p.run();
            //SerialHelper serialHelper = new SerialHelper(exoHand, device, rate);
            //serialHelper.run();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
