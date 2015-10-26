package net.fortytwo.extendo.demos.eval;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.SesameStream;
import edu.rpi.twc.sesamestream.impl.QueryEngineImpl;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.rdf.Activities;
import net.fortytwo.smsn.rdf.vocab.SmSnActivityOntology;
import net.fortytwo.smsn.rdf.vocab.Timeline;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.query.BindingSet;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParseException;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NewSesameStreamEvaluation {
    private static final Logger logger = SemanticSynchrony.getLogger(NewSesameStreamEvaluation.class);

    private static final String
            DEFAULT_NS = "http://example.org/defaultNs/";

    private static final int HANDSHAKE_TTL = 1; // seconds; 1s is the minimum TTL

    public static final String QUERY_FOR_HANDSHAKE_COMMON_ACQUAINTANCES
            = "PREFIX activity: <" + SmSnActivityOntology.NAMESPACE + ">\n" +
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
            "SELECT ?time ?actor1 ?actor2 ?person WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:recognitionTime ?instant .\n" +
            "  ?instant tl:at ?time .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?actor1 foaf:knows ?person .\n" +
            "  ?actor2 foaf:knows ?person .\n" +
            "  FILTER(str(?actor1) < str(?actor2))\n" +
            "}";

    // note: DBLP's dc:subject here instead of PKB's sioc:topic
    // note: SP2Bench's dc:creator here instead of PKB's foaf:maker
    public static final String QUERY_FOR_HANDSHAKE_COMMON_TOPICS
            = "PREFIX activity: <" + SmSnActivityOntology.NAMESPACE + ">\n" +
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "PREFIX dc: <http://purl.org/dc/elements/1.1/>\n" +
            "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
            "SELECT ?time ?actor1 ?actor2 ?topic WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:recognitionTime ?instant .\n" +
            "  ?instant tl:at ?time .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?paper1 dc:creator ?actor1 .\n" +
            "  ?paper1 dc:subject ?topic .\n" +
            "  ?paper2 dc:creator ?actor2 .\n" +
            "  ?paper2 dc:subject ?topic .\n" +
            "  FILTER(str(?actor1) < str(?actor2))\n" +
            "}";

    private static final String
            FRIENDS = "friends",
            TOPICS = "topics";

    private static final Random random;

    static {
        random = new Random();
        random.setSeed(System.currentTimeMillis());
    }

    private final List<Resource> people = new ArrayList<Resource>();

    private final QueryEngineImpl queryEngine;
    private final List<EvalClient> clients;

    private int averageSecondsBetweenHandshakes;
    private int totalPeople;
    private int totalThreads;
    private int timeLimitSeconds;
    private Set<String> queries;

    public NewSesameStreamEvaluation() {
        clients = new LinkedList<EvalClient>();
        //SesameStream.setDoPerformanceMetrics(true);
        queryEngine = new QueryEngineImpl();
    }

    public void initialize()
            throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException,
            RDFParseException, RDFHandlerException {
        loadPeople();

        int ppt = totalPeople/totalThreads;
        int extra = totalPeople - ppt*totalThreads;
        for (int i = 0; i < totalThreads; i++) {
            int length = (totalThreads - 1) == i ? ppt : ppt + extra;
            Resource[] peopleInThread = new Resource[length];
            for (int j = 0; j < length; j++) {
                peopleInThread[j] = people.get(i * ppt + j);
            }
            EvalClient client = new EvalClient(i, peopleInThread);
            clients.add(client);
        }

        // first, add queries
        if (queries.contains(FRIENDS)) {
            logger.info("adding 'friends in common' query");
            queryEngine.addQuery(SesameStream.INFINITE_TTL, QUERY_FOR_HANDSHAKE_COMMON_ACQUAINTANCES,
                    new BindingSetHandler() {
                        @Override
                        public void handle(BindingSet bindingSet) {
                            Value person = bindingSet.getValue("person");
                            handleHandshakeResult(bindingSet, FRIENDS, person);
                        }
                    });
        }
        if (queries.contains(TOPICS)) {
            logger.info("adding 'topics in common' query");
            queryEngine.addQuery(SesameStream.INFINITE_TTL, QUERY_FOR_HANDSHAKE_COMMON_TOPICS,
                    new BindingSetHandler() {
                        @Override
                        public void handle(BindingSet bindingSet) {
                            Value topic = bindingSet.getValue("topic");
                            handleHandshakeResult(bindingSet, TOPICS, topic);
                        }
                    });
        }

        // add data after queries
        addStaticData(totalPeople);
    }

    private void loadPeople() throws IOException {
        File f = new File("/tmp/sp2bench/people.txt");
        if (!f.exists()) {
            throw new IllegalStateException();
        }
        InputStream in = new FileInputStream(f);
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            String line;
            while (null != (line = br.readLine())) {
                //String[] a = line.trim().split(":");
                //if (2 != a.length) {
                //    throw new IllegalStateException();
                //}
                URI p = new URIImpl("urn:bnode:" + line.trim());
                people.add(p);
                //people.add(new BNodeImpl(a[1]));
            }
        } finally {
            in.close();
        }
    }

    // note: this method may be called in a thread other than the one in which the
    // simulation loop is running.
    private void handleHandshakeResult(BindingSet bindingSet, String queryName, Value... otherValues) {
        long now = System.currentTimeMillis();
        long timestamp;

        Value actor1 = bindingSet.getValue("actor1");
        Value actor2 = bindingSet.getValue("actor2");

        // always output the actors in lexicographic order
        if (actor1.stringValue().compareTo(actor2.stringValue()) > 0) {
            Value tmp = actor1;
            actor1 = actor2;
            actor2 = tmp;
        }

        Value timeValue = bindingSet.getValue("time");
        if (null == timeValue) {
            logger.severe("no time in " + bindingSet);
            return;
        }
        try {
            timestamp = Activities.TIMESTAMP_FORMAT.parse(timeValue.stringValue()).getTime();
        } catch (Exception t) {
            logger.log(Level.WARNING, "count not parse as dateTime: " + timeValue.stringValue()
                    + " in solution " + bindingSet);
            return;
        }

        StringBuilder sb = new StringBuilder();
        sb.append(now).append("\t").append("MATCH\t").append(queryName).append("\t").append(timestamp).append("\t")
                .append(actor1.stringValue()).append("\t").append(actor2.stringValue());
        for (Value v : otherValues) {
            sb.append("\t").append(v.stringValue());
        }
        System.out.println(sb);
    }

    private void addStaticData(int totalPeople) throws IOException, RDFParseException, RDFHandlerException {
        long before = System.currentTimeMillis();

        RDFParser p = Rio.createParser(RDFFormat.NTRIPLES);
        p.setRDFHandler(queryEngine.createRDFHandler(0));

        File dir = new File("/tmp/sp2bench/" + totalPeople);
        if (!dir.exists() || !dir.isDirectory()) {
            throw new IllegalStateException();
        }

        for (File f : dir.listFiles()) {
            if (f.getName().endsWith(".nt")) {
                logger.info("loading " + f);
                InputStream in = new FileInputStream(f);
                try {
                    p.parse(in, DEFAULT_NS);
                } finally {
                    in.close();
                }
            }
        }

        long after = System.currentTimeMillis();
        logger.info("loaded static dataset in " + (after - before) + "ms");
    }

    public void runSimulation() throws InterruptedException {
        for (EvalClient client : clients) {
            new Thread(client).start();
        }

        // assuming starting the clients takes no time
        Thread.sleep(0 < timeLimitSeconds ? timeLimitSeconds * 1000L : Long.MAX_VALUE);

        for (EvalClient client : clients) {
            client.stop();
        }
    }

    public void shutDown() {
        queryEngine.shutDown();
    }

    /**
     * @param frequency average frequency of events, in 1/milliseconds
     * @return time until the next event, in milliseconds
     */
    private long timeToNextEvent(double frequency) {
        return (long) (-Math.log(1.0 - random.nextDouble()) / frequency);
    }

    private static Statement[] toArray(final Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
    }

    private static void mainPrivate(final String[] args)
            throws QueryEngine.IncompatibleQueryException, IOException, QueryEngine.InvalidQueryException,
            InterruptedException, RDFParseException, RDFHandlerException {

        Options options = new Options();

        Option threadsOpt = new Option("t", "threads", true, "number of worker threads (default: 1)");
        threadsOpt.setArgName("THREADS");
        threadsOpt.setRequired(false);
        options.addOption(threadsOpt);

        Option peopleOpt = new Option("p", "people", true, "total number of people (default: 100)");
        peopleOpt.setArgName("PEOPLE");
        peopleOpt.setRequired(false);
        options.addOption(peopleOpt);

        Option queriesOpt = new Option("q", "queries", true, "queries (default: friends,topics)");
        queriesOpt.setArgName("QUERIES");
        queriesOpt.setRequired(false);
        options.addOption(queriesOpt);

        Option shakeTimeOpt = new Option("T", "timeBetweenShakes", true,
                "average time between handshakes, in seconds (default: 180)");
        shakeTimeOpt.setArgName("SECONDS");
        shakeTimeOpt.setRequired(false);
        options.addOption(shakeTimeOpt);

        Option limitOpt = new Option("l", "limit", true, "time limit in seconds");
        limitOpt.setArgName("SECONDS");
        limitOpt.setRequired(false);
        options.addOption(limitOpt);

        Option verboseOpt = new Option("v", "verbose", false, "verbose output");
        verboseOpt.setRequired(false);
        options.addOption(verboseOpt);

        CommandLineParser clp = new PosixParser();
        CommandLine cmd = null;

        try {
            cmd = clp.parse(options, args);
        } catch (org.apache.commons.cli.ParseException e) {
            printUsageAndExit(options);
        }

        int nThreads = Integer.valueOf(cmd.getOptionValue(threadsOpt.getOpt(), "1"));
        int totalPeople = Integer.valueOf(cmd.getOptionValue(peopleOpt.getOpt(), "100"));
        String queriesStr = cmd.getOptionValue(queriesOpt.getOpt(), "friends,topics");
        int timeLimitSeconds = Integer.valueOf(cmd.getOptionValue(limitOpt.getOpt(), "0"));
        int shakeTime = Integer.valueOf(cmd.getOptionValue(shakeTimeOpt.getOpt(), "180"));
        boolean verbose = cmd.hasOption(verboseOpt.getOpt());

        Set<String> queries = new HashSet<String>();
        for (String q : queriesStr.split(",")) {
            String query = q.trim();
            if (0 == query.length()) {
                printUsageAndExit(options);
            }
            queries.add(query);
        }

        NewSesameStreamEvaluation eval = new NewSesameStreamEvaluation();
        //eval.peoplePerThread = 100;
        //eval.totalThreads = 1;
        //eval.averageSecondsBetweenHandshakes = 3 * 60;
        //eval.timeLimitSeconds = 5 * 60;
        //eval.queries.add(FRIENDS);
        //eval.queries.add(TOPICS);
        eval.totalPeople = totalPeople;
        eval.totalThreads = nThreads;
        eval.averageSecondsBetweenHandshakes = shakeTime;
        eval.timeLimitSeconds = timeLimitSeconds;
        eval.queries = queries;

        eval.initialize();
        eval.runSimulation();
        eval.shutDown();

        logger.info("exiting simulation");
    }

    private static void printUsageAndExit(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("sesamestream-eval", options);
        //System.out.println("options: " + options.toString());
        //System.err.println("see source for usage");
        System.exit(1);
    }

    private class EvalClient implements Runnable {
        private final int id;
        private long lastEvent;
        private boolean stopped;
        private final Resource[] localPeople;
        private final double averageFrequency;

        private EvalClient(final int id,
                           final Resource[] localPeople) {
            logger.info("creating client " + id + " with " + localPeople.length + " people");
            this.id = id;
            this.localPeople = localPeople;

            averageFrequency = localPeople.length / (2.0 * averageSecondsBetweenHandshakes * 1000);
        }

        private void shake(long now) throws IOException {
            // choose two random, distinct people from this group.
            // They can be the same as a previous pair.
            int pid1 = random.nextInt(localPeople.length);
            int pid2 = (pid1 + 1 + random.nextInt(localPeople.length - 1)) % localPeople.length;

            Resource a1 = localPeople[pid1];
            Resource a2 = localPeople[pid2];

            // note: the logged timestamp is identical to the event timestamp represented in RDF
            System.out.println(now + "\tEVENT\t" + this.id + "\t"
                    + a1.stringValue() + "\t" + a2.stringValue());

            Dataset d = Activities.datasetForHandshakeInteraction(now, a1, a2);
            queryEngine.addStatements(HANDSHAKE_TTL, toArray(d));
        }

        private void waitAndShake() throws InterruptedException, IOException {
            // use clock time as simulation time
            long now = System.currentTimeMillis();

            long delay = timeToNextEvent(averageFrequency);
            long nextEvent = lastEvent + delay;
            if (lastEvent > 0) {
                if (nextEvent > now) {
                    Thread.sleep(nextEvent - now);
                } else {
                    // note the ratio of DELAY_TOO_SHORT to EVENT
                    System.out.println(now + "\tDELAY_TOO_SHORT\t" + this.id + "\t" + delay);
                }
            }

            // note: this may not be exactly equal to nextEvent
            lastEvent = System.currentTimeMillis();

            shake(lastEvent);
        }

        public void run() {
            try {
                while (!stopped) {
                    waitAndShake();
                }
            } catch (Exception t) {
                logger.log(Level.SEVERE, "eval client failed", t);
            }
            logger.info("eval client " + id + " exited");
        }

        public void stop() {
            long now = System.currentTimeMillis();
            System.out.println(now + '\t' + id + "\tSTOPPED");
            stopped = true;
        }
    }

    /*
      ./new-sesamestream-evaluation.sh -l 30 -p 100 -t 2 -q friends
     */
    public static void main(final String[] args) {
        try {
            mainPrivate(args);
        } catch (Exception t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
