package net.fortytwo.extendo.demos.eval;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.impl.QueryEngineImpl;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.rdf.Activities;
import net.fortytwo.extendo.rdf.vocab.ExtendoActivityOntology;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import net.fortytwo.extendo.rdf.vocab.Timeline;
import net.fortytwo.rdfagents.model.Dataset;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.model.vocabulary.OWL;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A simulation of a variable number of people moving between a variable number of rooms, as in a conference center
 * equipped with Wi-Fi triangulation, occasionally shaking hands with other conference-goers.
 * A SesameStream continuous SPARQL query engine combines event data from individuals to recognize possible handshakes
 * and identify relationships between the conference-goers.
 * The task of event processing is broken up by room to a variable number of worker threads.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SesameStreamEvaluation {
    private static final Logger logger = Extendo.getLogger(SesameStreamEvaluation.class);

    private static final String EX = "http://example.org/";

    private static final int
            QUERY_TTL = 0,
            MAX_HALF_HANDSHAKE_TTL = 6,
            HANDSHAKE_TTL = 3;

    private static final int
            AVERAGE_SECONDS_BETWEEN_MOVES = 5 * 60,
            AVERAGE_SECONDS_BETWEEN_HANDSHAKES = 3 * 60;

    private static final int
            MAX_PAPERS_PER_PERSON = 20,
            MAX_TOPICS_PER_PAPER = 5;

    private static final double
            PROB_TOPICS_IN_COMMON = 0.8;

    // a set of handshakes, accessed by multiple threads, for distinguishing actual shakes from false positives
    private Set<String> handshakesInProgress = newConcurrentSet();

    // e.g. 2015-02-22T01:35:10-0500
    private static final DateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ssZ");

    //*
    private static final String QUERY_FOR_HANDSHAKE_PAIRS
            = "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
            "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
            "SELECT ?actor1 ?actor2 ?room ?time1 ?time2 WHERE {\n" +
            "  ?a1 a activity:BatonGesture .\n" +
            "  ?a1 activity:actor ?actor1 .\n" +
            "  ?a1 activity:recognitionTime ?instant1 .\n" +
            "  ?instant1 tl:at ?time1 .\n" +
            "  ?a2 a activity:BatonGesture .\n" +
            "  ?a2 activity:actor ?actor2 .\n" +
            "  ?a2 activity:recognitionTime ?instant2 .\n" +
            "  ?instant2 tl:at ?time2 .\n" +
            "  ?actor1 activity:locatedAt ?room .\n" +
            "  ?actor2 activity:locatedAt ?room .\n" +
            "  FILTER(?actor1 != ?actor2)\n" +
            "}";
    //*/

    private static final String QUERY_FOR_HANDSHAKES
            = "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
            "  PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
            "  SELECT ?actor1 ?actor2 ?time WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?a activity:recognitionTime ?instant .\n" +
            "  ?instant tl:at ?time .\n" +
            "  FILTER(?actor1 != ?actor2)\n" +
            "}";

    private static final String QUERY_FOR_HANDSHAKE_COMMON_TOPICS
            = "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "PREFIX sioc: <http://rdfs.org/sioc/ns#>\n" +
            "SELECT ?actor1 ?actor2 ?topic WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?paper1 foaf:maker ?actor1 .\n" +
            "  ?paper1 sioc:topic ?topic .\n" +
            "  ?paper2 foaf:maker ?actor2 .\n" +
            "  ?paper2 sioc:topic ?topic .\n" +
            "  FILTER(?actor1 != ?actor2)\n" +
            "}";

    private static final String QUERY_FOR_HANDSHAKE_COMMON_ACQUAINTANCES
            = "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ?actor1 ?actor2 ?person WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?actor1 foaf:knows ?person .\n" +
            "  ?actor2 foaf:knows ?person .\n" +
            "  FILTER(?actor1 != ?actor2)\n" +
            "}";

    private final QueryEngine queryEngine;
    private final ValueFactory vf;

    private final Random random = new Random();

    private final Person[] people;
    private final Room[] rooms;
    private final Topic[] topics;

    private ThreadLocal<Long> timeOfLastPulse = new ThreadLocal<Long>();
    private ThreadLocal<Long> timeOfLastHandshake = new ThreadLocal<Long>();

    private int
            totalMoves,
            totalHalfshakes,
            totalShakes,
            totalReceivedHalfshakePairs,
            totalReceivedTruePositiveHalfshakePairs,
            totalReceivedHandshakesWithCommonTopics,
            totalReceivedHandshakesWithCommonKnows;

    private final Object printMutex = "";

    public SesameStreamEvaluation(final int totalThreads,
                                  final int totalPeople,
                                  final int totalRooms,
                                  final Set<String> queries,
                                  final int timeLimitSeconds)
            throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException {

        if (totalThreads < 1) {
            throw new IllegalArgumentException("invalid number of threads: " + totalThreads);
        }

        if (timeLimitSeconds < 0) {
            throw new IllegalArgumentException();
        }

        if (totalRooms < 2) {
            throw new IllegalArgumentException();
        }

        // these give around 50% probability of a common acquaintance
        int[] x = new int[]{0, 100, 200, 500, 1000};
        int[] y = new int[]{0, 8, 12, 18, 25};
        Integer p = null;
        for (int i = 0; i < x.length; i++) {
            if (x[i] == totalPeople) {
                p = y[i];
                break;
            } else if (x[i] > totalPeople) {
                p = y[i - 1] + (int) (((totalPeople - x[i]) / (1.0 * (x[i] - x[i - 1]))) * (y[i] - y[i - 1]));
                break;
            }
        }
        int maxPeopleKnown = null == p ? (int) (y[x.length - 1] * totalPeople / (1.0 * x[x.length - 1])) : p;

        //SesameStream.setDoPerformanceMetrics(true);

        queryEngine = new QueryEngineImpl();
        vf = new ValueFactoryImpl();

        queryEngine.addQuery(QUERY_TTL, QUERY_FOR_HANDSHAKE_PAIRS, new BindingSetHandler() {
            @Override
            public void handle(BindingSet bindingSet) {
                totalReceivedHalfshakePairs++;

                long now = System.currentTimeMillis();
                long time1;
                Value timeValue = bindingSet.getValue("time1");
                if (null == timeValue) {
                    logger.warning("no time1 in " + bindingSet);
                    return;
                }
                try {
                    time1 = DATE_FORMAT.parse(timeValue.stringValue()).getTime();
                } catch (Throwable t) {
                    logger.log(Level.WARNING, "count not parse as dateTime: " + timeValue.stringValue()
                            + " in solution " + bindingSet, t);
                    return;
                }

                String actor1 = bindingSet.getValue("actor1").stringValue();
                String actor2 = bindingSet.getValue("actor2").stringValue();
                String a1 = actor1.substring(actor1.lastIndexOf("n") + 1);
                String a2 = actor2.substring(actor2.lastIndexOf("n") + 1);

                if (Integer.valueOf(a1).compareTo(Integer.valueOf(a2)) > 0) {
                    String tmp = a1;
                    a1 = a2;
                    a2 = tmp;
                }
                String key = "" + time1 + ":" + a1 + ":" + a2;

                if (handshakesInProgress.remove(key)) {
                    totalReceivedTruePositiveHalfshakePairs++;

                    findPulseLatency(now);

                    System.out.println("handshake pair: " + bindingSet);
                    Person person1 = people[Integer.valueOf(a1)];
                    Person person2 = people[Integer.valueOf(a2)];
                    try {
                        handshake(person1, person2);
                    } catch (IOException e) {
                        throw new IllegalStateException(e);
                    }
                } else {
                    System.out.println("no such handshake: " + key);
                }
            }
        });

        if (queries.contains("friends")) {
            queryEngine.addQuery(QUERY_TTL, QUERY_FOR_HANDSHAKE_COMMON_ACQUAINTANCES, new BindingSetHandler() {
                @Override
                public void handle(BindingSet bindingSet) {
                    totalReceivedHandshakesWithCommonKnows++;

                    findHandshakeLatency(System.currentTimeMillis());

                    System.out.println("GOT A 'KNOWS' HANDSHAKE: " + bindingSet);
                }
            });
        }

        if (queries.contains("topics")) {
            queryEngine.addQuery(QUERY_TTL, QUERY_FOR_HANDSHAKE_COMMON_TOPICS, new BindingSetHandler() {
                @Override
                public void handle(BindingSet bindingSet) {
                    totalReceivedHandshakesWithCommonTopics++;

                    findHandshakeLatency(System.currentTimeMillis());

                    System.out.println("GOT A 'TOPICS' HANDSHAKE: " + bindingSet);
                }
            });
        }

        double averagePeopleKnown = (1 + maxPeopleKnown) / 2;
        double averagePapersPerPerson = (1 + MAX_PAPERS_PER_PERSON) / 2;
        double averageTopicsPerPaper = (1 + MAX_TOPICS_PER_PAPER) / 2;
        double averageTopicsPerPerson = averagePapersPerPerson * averageTopicsPerPaper;
        int nTopics = (int) (averageTopicsPerPerson
                / (1 - Math.pow(1 - PROB_TOPICS_IN_COMMON, 1.0 / averageTopicsPerPerson)));

        rooms = new Room[totalRooms];
        for (int i = 0; i < totalRooms; i++) {
            rooms[i] = new Room(i);
        }

        topics = new Topic[nTopics];
        for (int i = 0; i < nTopics; i++) {
            topics[i] = new Topic(i);
        }

        int totalPapers = 0;
        int totalTopics = 0;
        int totalKnown = 0;

        long startTime = System.currentTimeMillis();
        people = new Person[totalPeople];
        for (int i = 0; i < totalPeople; i++) {
            Person person = new Person(i, startTime);
            people[i] = person;

            // every person has at least one paper and at most MAX_PAPERS
            int nPersonPapers = 1 + random.nextInt(MAX_PAPERS_PER_PERSON);
            for (int j = 0; j < nPersonPapers; j++) {
                Paper paper = new Paper(totalPapers++);
                person.papers.add(paper);

                // each paper has at least one topic and at most MAX_TOPICS
                int nPaperTopics = 1 + random.nextInt(MAX_TOPICS_PER_PAPER);
                for (int k = 0; k < nPaperTopics; k++) {
                    Topic topic = topics[random.nextInt(nTopics)];
                    paper.topics.add(topic);
                    totalTopics++;
                }
            }

            // initial, uniform distribution of people over rooms
            person.moveTo(randomRoom(), startTime);
        }
        for (Person person : people) {
            int nKnown = 1 + random.nextInt(maxPeopleKnown);
            for (int j = 0; j < nKnown; j++) {
                Person other = people[random.nextInt(totalPeople)];
                person.known.add(other);
            }
            totalKnown += nKnown;
        }

        System.out.println("total people: " + totalPeople);
        System.out.println("total rooms: " + totalRooms);
        System.out.println("total papers: " + totalPapers);
        System.out.println("total topics: " + nTopics);
        System.out.println("maximum people known: " + maxPeopleKnown);
        System.out.println("average papers per person (projected): " + averagePapersPerPerson);
        System.out.println("average papers per person (actual): " + (totalPapers / (1.0 * totalPeople)));
        System.out.println("average topics per paper (projected): " + averageTopicsPerPaper);
        System.out.println("average topics per paper (actual): " + (totalTopics / (1.0 * totalPapers)));
        System.out.println("average topics per person (projected): " + averageTopicsPerPerson);
        System.out.println("average topics per person (actual): " + (totalTopics / (1.0 * totalPeople)));
        System.out.println("average people known (projected): " + averagePeopleKnown);
        System.out.println("average people known (actual): " + (totalKnown / (1.0 * totalPeople)));
        System.out.println("probability of topics in common: " + PROB_TOPICS_IN_COMMON);

        // add static metadata
        String siocNs = "http://rdfs.org/sioc/ns#";
        URI siocTopic = vf.createURI(siocNs + "topic");
        for (Person person : people) {
            queryEngine.addStatements(0, vf.createStatement(person.uri, RDF.TYPE, FOAF.PERSON));
            for (Paper paper : person.papers) {
                queryEngine.addStatements(0,
                        vf.createStatement(paper.uri, RDF.TYPE, FOAF.DOCUMENT),
                        vf.createStatement(paper.uri, FOAF.MAKER, person.uri));
                // note: we assume no papers in common
                for (Topic topic : paper.topics) {
                    queryEngine.addStatements(0,
                            vf.createStatement(topic.uri, RDF.TYPE, OWL.THING),
                            vf.createStatement(paper.uri, siocTopic, topic.uri));
                }
            }

            // each person knows at least one other person and at most MAX_PEOPLE_KNOWN
            for (Person other : person.known) {
                queryEngine.addStatements(0, vf.createStatement(person.uri, FOAF.KNOWS, other.uri));
            }
        }

        ScheduledExecutorService service = Executors.newSingleThreadScheduledExecutor();

        if (0 < timeLimitSeconds) {
            service.schedule(new Runnable() {
                @Override
                public void run() {
                    logger.info("quitting simulation after configured time limit of " + timeLimitSeconds + "s");
                    System.exit(0);
                }
            }, timeLimitSeconds, TimeUnit.SECONDS);
        }

        /*
        service.scheduleWithFixedDelay(new Runnable() {
            public void run() {
                System.out.println("queryEngine: " + queryEngine);
                System.out.println("break here");
            }
        }, initialDelay, 60, TimeUnit.SECONDS);
        //*/

        startTime = System.currentTimeMillis();

        for (int i = 0; i < totalThreads; i++) {
            int fromRoom = i * (totalRooms / totalThreads);
            int toRoom = i == totalThreads - 1 ? totalRooms - 1 : (i + 1) * (totalRooms / totalThreads) - 1;
            Simulation sim = new Simulation(i, startTime, totalPeople, fromRoom, toRoom);
            new Thread(sim).start();
        }
    }

    private synchronized void findPulseLatency(final long now) {
        Long then = timeOfLastPulse.get();
        if (null != then) {
            long latency = now - then;
            System.out.println("pulse latency = " + latency + "ms");
            timeOfLastPulse.set(null);
        }
    }

    private synchronized void findHandshakeLatency(final long now) {
        Long then = timeOfLastHandshake.get();
        if (null != then) {
            long latency = now - then;
            System.out.println("handshake latency = " + latency + "ms");
            timeOfLastHandshake.set(null);
        }
    }

    private class Simulation implements Runnable {
        private final int index;
        private final long startTime;
        private final int totalPeople;
        private final int fromRoom, toRoom;

        private Simulation(int index, long startTime, int totalPeople, int fromRoom, int toRoom) {
            logger.info("creating simulation #" + index + " [" + fromRoom + ", " + toRoom + "]");
            this.index = index;
            this.startTime = startTime;
            this.totalPeople = totalPeople;
            this.fromRoom = fromRoom;
            this.toRoom = toRoom;
        }

        @Override
        public void run() {
            logger.info("running simulation #" + index);
            long lastTimeStep = startTime;
            long lastReport = startTime;
            try {
                while (true) {
                    long now = System.currentTimeMillis();
                    long elapsed = now - lastTimeStep;

                    // prevent cycles from becoming arbitrarily short and busy
                    if (elapsed < 1000) {
                        Thread.sleep(1000 - elapsed);
                        now = System.currentTimeMillis();
                        elapsed = now - lastTimeStep;
                    }

                    lastTimeStep = now;

                    int presenceTtl = 1 + (int) ((elapsed * 1.5) / 1000);
                    if (presenceTtl > 300) {
                        throw new IllegalStateException("presence TTL is too large: " + presenceTtl);
                    }

                    long moveTime = 0, shakeTime = 0;
                    for (int i = fromRoom; i <= toRoom; i++) {
                        Room room = rooms[i];
                        for (Person person : room.people) {
                            try {
                                long before = System.currentTimeMillis(), after;
                                person.considerShakingHands(now);
                                after = System.currentTimeMillis();
                                shakeTime += (after - before);

                                // note: move last, as this may put the person under the control of another thread
                                before = after;
                                person.considerMoving(now, presenceTtl);
                                after = System.currentTimeMillis();
                                moveTime += (after - before);
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        }
                    }

                    long after = System.currentTimeMillis();
                    long time = after - now;

                    synchronized (printMutex) {
                        System.out.println("thread #" + index + " cycle from " + now + " to " + after + " took " + time + "ms");
                        System.out.println("spent at most " + moveTime + "ms on moves and " + shakeTime + "ms on shakes");
                        /*
                        // output detailed stats only so often
                        lastReport = after;
                        if (after - lastReport >= 5000) {
                            double movesPerSecond = totalMoves * 1000.0 / time;
                            double halfshakesPerSecond = totalHalfshakes * 1000.0 / time;
                            double shakesPerSecond = totalShakes * 1000.0 / time;
                            double receivedHalfshakesPerSecond = (totalReceivedHalfshakePairs / 2) * 1000.0 / time;
                            double receivedHandshakesWithCommonTopicsPerSecond
                                    = (totalReceivedHandshakesWithCommonTopics / 2) * 1000.0 / time;
                            double receivedHandshakesWithCommonKnowsPerSecond
                                    = (totalReceivedHandshakesWithCommonKnows / 2) * 1000.0 / time;
                            double movePeriod = totalPeople * time / (totalMoves * 1000.0);
                            double halfshakePeriod = totalPeople * time / (totalHalfshakes * 1000.0);
                            double shakePeriod = totalPeople * time / (totalShakes * 1000.0);
                            double receivedHalfshakePeriod = totalPeople * time / ((totalReceivedHalfshakePairs / 2) * 1000.0);
                            double receivedTruePositiveHalfshakePeriod = totalPeople * time
                                    / (totalReceivedTruePositiveHalfshakePairs * 1000.0);
                            double receivedHandshakesWithCommonTopicsPeriod = totalPeople * time
                                    / ((totalReceivedHandshakesWithCommonTopics / 2) * 1000.0);
                            double receivedHandshakesWithCommonKnowsPeriod = totalPeople * time
                                    / ((totalReceivedHandshakesWithCommonKnows / 2) * 1000.0);

                            System.out.println("average moves per second: " + movesPerSecond);
                            System.out.println("average half-shakes per second: " + halfshakesPerSecond);
                            System.out.println("average shakes per second: " + shakesPerSecond);
                            System.out.println("average received half-shakes per second: " + receivedHalfshakesPerSecond);
                            System.out.println("average received handshakes with common topics per second: "
                                    + receivedHandshakesWithCommonTopicsPerSecond);
                            System.out.println("average received handshakes with common knows per second: "
                                    + receivedHandshakesWithCommonKnowsPerSecond);
                            System.out.println("average seconds between moves, per person: " + movePeriod);
                            System.out.println("average seconds between half-shakes, per person: " + halfshakePeriod);
                            System.out.println("average seconds between shakes, per person: " + shakePeriod);
                            System.out.println("average seconds between received half-shake pairs, per person: "
                                    + receivedHalfshakePeriod);
                            System.out.println("average seconds between received true positive half-shake pairs, per person: "
                                    + receivedTruePositiveHalfshakePeriod);
                            System.out.println("average seconds between handshakes with common topics, per person: "
                                    + receivedHandshakesWithCommonTopicsPeriod);
                            System.out.println("average seconds between handshakes with common knows, per person: "
                                    + receivedHandshakesWithCommonKnowsPeriod);
                        }
                        //*/
                    }
                }
            } catch (Throwable t) {
                logger.log(Level.SEVERE, "simulation thread died with error", t);
            }
        }
    }

    // star topology minimizes repeat handshakes
    private Room randomRoom() {
        return rooms[random.nextInt(rooms.length)];
    }

    private String handshakeKey(final long now, final Person person1, final Person person2) {
        int id1, id2;
        if (person1.id < person2.id) {
            id1 = person1.id;
            id2 = person2.id;
        } else {
            id1 = person2.id;
            id2 = person1.id;
        }

        return "" + 1000L * (now / 1000L) + ":" + id1 + ":" + id2;
    }

    private void handshake(final Person person1, final Person person2) throws IOException {
        totalShakes++;

        System.out.println(person1 + " SHAKES with " + person2);

        long now = System.currentTimeMillis();

        Dataset d = Activities.datasetForHandshakeInteraction(now, person1.uri, person2.uri);

        timeOfLastHandshake.set(System.currentTimeMillis());
        queryEngine.addStatements(HANDSHAKE_TTL, toArray(d));
    }

    private void halfHandshakes(final Person person1, final Person person2) throws IOException {
        totalHalfshakes++;

        long now = System.currentTimeMillis();

        String key = handshakeKey(now, person1, person2);
        if (handshakesInProgress.contains(key)) {
            throw new IllegalStateException();
        }
        handshakesInProgress.add(key);
        System.out.println(person1 + " half-shakes with " + person2 + " --> " + handshakesInProgress.size() + " " + key);

        // note: the fact that the gestures occur at *exactly* the same moment is unimportant
        Dataset d1 = Activities.datasetForBatonGesture(now, person1.uri);
        Dataset d2 = Activities.datasetForBatonGesture(now, person2.uri);

        timeOfLastPulse.set(System.currentTimeMillis());
        queryEngine.addStatements(person1.halfHandshakeTtl, toArray(d1));
        queryEngine.addStatements(person2.halfHandshakeTtl, toArray(d2));
    }

    private void presence(final Person person, final Room room, final int ttl) throws IOException {
        Statement st = vf.createStatement(
                person.uri, vf.createURI(ExtendoActivityOntology.NAMESPACE + "locatedAt"), room.uri);
        queryEngine.addStatements(ttl, st);
    }

    private Statement[] toArray(final Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
    }

    private <T> Set<T> newConcurrentSet() {
        return Collections.newSetFromMap(new ConcurrentHashMap<T, Boolean>());
    }

    private abstract class Thing {
        protected final int id;
        protected final URI uri;

        private Thing(final int id) {
            this.id = id;
            this.uri = new URIImpl(EX + type() + id);
        }

        protected abstract String type();

        @Override
        public String toString() {
            return type() + id;
        }

        @Override
        public boolean equals(final Object other) {
            return other instanceof Thing && ((Thing) other).uri.equals(uri);
        }

        @Override
        public int hashCode() {
            return uri.hashCode();
        }
    }

    private class Person extends Thing {
        private final int halfHandshakeTtl;
        private final Collection<Paper> papers = new LinkedList<Paper>();
        private final Collection<Person> known = new LinkedList<Person>();

        private Long
                timeOfLastMove,
                timeOfLastHandshake;
        private long
                timeLastConsideredMove,
                timeLastConsideredHandshake;

        private Room currentRoom;

        private Person(final int id,
                       final long startTime) {
            super(id);

            // note: avoids 0 as a value of TTL
            this.halfHandshakeTtl = 1 + random.nextInt(MAX_HALF_HANDSHAKE_TTL);

            this.timeLastConsideredMove = startTime;
            this.timeLastConsideredHandshake = startTime;
        }

        @Override
        protected String type() {
            return "person";
        }

        public void moveTo(final Room room, long now) throws IOException {
            System.out.println(this + " is moving to " + room
                    + (null == timeOfLastMove ? "" : (" after " + (now - timeOfLastMove) / 1000) + "s dwell time"));
            timeOfLastMove = now;

            if (null != currentRoom) {
                currentRoom.people.remove(this);
            }

            currentRoom = room;
            currentRoom.people.add(this);
        }

        public void considerMoving(long now, int ttl) throws IOException {
            long elapsed = now - timeLastConsideredMove;
            timeLastConsideredMove = now;
            double probMove = 1 - Math.pow(0.5,
                    elapsed / (AVERAGE_SECONDS_BETWEEN_MOVES * 1000.0));

            if (random.nextDouble() < probMove) {
                Room newRoom;
                do {
                    newRoom = randomRoom();
                } while (newRoom == currentRoom);

                totalMoves++;
                moveTo(newRoom, now);
            }

            // refresh the person's current position, regardless of whether the person moved
            presence(this, currentRoom, ttl);
        }

        public void considerShakingHands(long now) throws IOException {
            long elapsed = now - timeLastConsideredHandshake;
            timeLastConsideredHandshake = now;
            double probHandshake = 1 - Math.pow(0.5,
                    elapsed / (AVERAGE_SECONDS_BETWEEN_HANDSHAKES * 1000.0));

            if (random.nextDouble() < probHandshake) {
                if (currentRoom.people.size() > 0) {
                    System.out.println(this + " is shaking hands"
                            + (null == timeOfLastHandshake ? "" : (" after " + (now - timeOfLastHandshake) / 1000) + "s idle time"));
                    timeOfLastHandshake = now;

                    // choose a single person in the newly entered space to shake hands with
                    Person other = currentRoom.people.iterator().next();

                    halfHandshakes(this, other);
                }
            }
        }
    }

    private class Room extends Thing {
        // note: needs to be thread-safe so the simulator threads can iterate over people while moves are in progress
        private final Set<Person> people = newConcurrentSet();

        private Room(final int id) {
            super(id);
        }

        @Override
        protected String type() {
            return "room";
        }
    }

    private class Paper extends Thing {
        private final Collection<Topic> topics = new LinkedList<Topic>();

        public Paper(final int id) {
            super(id);
        }

        @Override
        protected String type() {
            return "paper";
        }
    }

    private class Topic extends Thing {
        public Topic(final int id) {
            super(id);
        }

        @Override
        protected String type() {
            return "topic";
        }
    }

    public static void main(final String[] args) throws Exception {
        /*
        Set<String> qs = new HashSet<String>();
        qs.add("topics");
        new SesameStreamEvaluation(1, 800, 8, qs, 0);
        if (true) return;
        */

        try {
            Options options = new Options();

            Option threadsOpt = new Option("t", "threads", true, "number of worker threads (default: 4)");
            threadsOpt.setArgName("THREADS");
            threadsOpt.setRequired(false);
            options.addOption(threadsOpt);

            Option peopleOpt = new Option("p", "people", true, "number of people (default: 100)");
            peopleOpt.setArgName("PEOPLE");
            peopleOpt.setRequired(false);
            options.addOption(peopleOpt);

            Option roomsOpt = new Option("r", "rooms", true, "number of rooms (default: 10)");
            roomsOpt.setArgName("ROOMS");
            roomsOpt.setRequired(false);
            options.addOption(roomsOpt);

            Option queriesOpt = new Option("q", "queries", true, "queries (default: topics)");
            queriesOpt.setArgName("QUERIES");
            queriesOpt.setRequired(false);
            options.addOption(queriesOpt);

            Option limitOpt = new Option("l", "limit", true, "time limit in seconds");
            limitOpt.setArgName("LIMIT");
            limitOpt.setRequired(false);
            options.addOption(limitOpt);

            CommandLineParser clp = new PosixParser();
            CommandLine cmd = null;

            try {
                cmd = clp.parse(options, args);
            } catch (org.apache.commons.cli.ParseException e) {
                printUsageAndExit();
            }

            int nThreads = Integer.valueOf(cmd.getOptionValue(threadsOpt.getOpt(), "4"));
            int nPeople = Integer.valueOf(cmd.getOptionValue(peopleOpt.getOpt(), "100"));
            int nRooms = Integer.valueOf(cmd.getOptionValue(roomsOpt.getOpt(), "8"));
            String queriesStr = cmd.getOptionValue(queriesOpt.getOpt(), "topics");
            int timeLimitSeconds = Integer.valueOf(cmd.getOptionValue(limitOpt.getOpt(), "0"));

            Set<String> queries = new HashSet<String>();
            for (String q : queriesStr.split(";")) {
                String query = q.trim();
                if (0 == query.length()) {
                    printUsageAndExit();
                }
                queries.add(query);
            }

            new SesameStreamEvaluation(nThreads, nPeople, nRooms, queries, timeLimitSeconds);
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void printUsageAndExit() {
        System.err.println("see source for usage");
        System.exit(1);
    }
}
