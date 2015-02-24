package net.fortytwo.extendo.demos.eval;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.impl.QueryEngineImpl;
import net.fortytwo.extendo.rdf.Activities;
import net.fortytwo.extendo.rdf.vocab.ExtendoActivityOntology;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import net.fortytwo.extendo.rdf.vocab.Timeline;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
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
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SesameStreamEvaluation {

    private static final String EX = "http://example.org/";

    private static final int
            QUERY_TTL = 0,
            MAX_HALF_HANDSHAKE_TTL = 10,
            HANDSHAKE_TTL = 5,
            PRESENCE_TTL = 20;

    private static final int
            TIME_STEP_MILLISECONDS = 15000,
            AVERAGE_SECONDS_BETWEEN_MOVES = 5 * 60,
            AVERAGE_SECONDS_BETWEEN_HANDSHAKES = 3 * 60;

    private static final int
            MAX_PAPERS = 20,
            MAX_TOPICS = 5,
            MAX_PEOPLE_KNOWN = 18 * 2, // use around 8 for 100 total people, 12:200, 18:500, 25:1000 for 50% probability
            TOTAL_PEOPLE = 500,
            TOTAL_PLACES = 10;

    private static final double
            PROB_TOPICS_IN_COMMON = 0.8,
            PROB_MOVE = 1 - Math.pow(0.5,
                    1.0 / (AVERAGE_SECONDS_BETWEEN_MOVES * 1000.0 / TIME_STEP_MILLISECONDS)),
            PROB_HANDSHAKE = 1 - Math.pow(0.5,
                    1.0 / (AVERAGE_SECONDS_BETWEEN_HANDSHAKES * 1000.0 / TIME_STEP_MILLISECONDS));

    private Set<String> handshakesInProgress = new HashSet<String>();

    // e.g. 2015-02-22T01:35:10-0500
    private static final DateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ssZ");

    //*
    private static final String QUERY_FOR_HANDSHAKE_PAIRS
            = "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
            "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
            "SELECT ?actor1 ?actor2 ?place ?time1 ?time2 WHERE {\n" +
            "  ?a1 a activity:BatonGesture .\n" +
            "  ?a1 activity:actor ?actor1 .\n" +
            "  ?a1 activity:recognitionTime ?instant1 .\n" +
            "  ?instant1 tl:at ?time1 .\n" +
            "  ?a2 a activity:BatonGesture .\n" +
            "  ?a2 activity:actor ?actor2 .\n" +
            "  ?a2 activity:recognitionTime ?instant2 .\n" +
            "  ?instant2 tl:at ?time2 .\n" +
            "  ?actor1 activity:locatedAt ?place .\n" +
            "  ?actor2 activity:locatedAt ?place .\n" +
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
    private final Place[] places;
    private final Topic[] topics;

    private final long startTime;
    private Long timeOfLastPulse, timeOfLastHandshake;

    private int
            totalMoves,
            totalHalfshakes,
            totalShakes,
            totalReceivedHalfshakePairs,
            totalReceivedTruePositiveHalfshakePairs,
            totalReceivedHandshakesWithCommonTopics,
            totalReceivedHandshakesWithCommonKnows;

    public SesameStreamEvaluation()
            throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException {
        //SesameStream.setDoPerformanceMetrics(true);

        queryEngine = new QueryEngineImpl();
        vf = new ValueFactoryImpl();

        queryEngine.addQuery(QUERY_TTL, QUERY_FOR_HANDSHAKE_PAIRS, new BindingSetHandler() {
            @Override
            public void handle(BindingSet bindingSet) {
                totalReceivedHalfshakePairs++;

                long now = System.currentTimeMillis();
                long time1;
                try {
                    time1 = DATE_FORMAT.parse(bindingSet.getValue("time1").stringValue()).getTime();
                } catch (ParseException e) {
                    throw new IllegalStateException(e);
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

                    if (null != timeOfLastPulse) {
                        long latency = now - timeOfLastPulse;
                        timeOfLastPulse = null;
                        System.out.println("pulse latency = " + latency + "ms");
                    }

                    System.out.println("handshake pair: " + bindingSet);
                    Person person1 = people[Integer.valueOf(a1)];
                    Person person2 = people[Integer.valueOf(a2)];
                    try {
                        handshake(person1, person2);
                    } catch (IOException e) {
                        throw new IllegalStateException(e);
                    }
                } else {
                    System.out.println("no such handshake: " + key + " " + bindingSet);
                }
            }
        });

        /*
        queryEngine.addQuery(QUERY_TTL, QUERY_FOR_HANDSHAKE_COMMON_ACQUAINTANCES, new BindingSetHandler() {
            @Override
            public void handle(BindingSet bindingSet) {
                totalReceivedHandshakesWithCommonKnows++;

                if (null != timeOfLastHandshake) {
                    long latency = System.currentTimeMillis() - timeOfLastHandshake;
                    timeOfLastHandshake = null;
                    System.out.println("handshake latency = " + latency + "ms");
                }

                System.out.println("GOT A 'KNOWS' HANDSHAKE: " + bindingSet);
            }
        });
        //*/

        queryEngine.addQuery(QUERY_TTL, QUERY_FOR_HANDSHAKE_COMMON_TOPICS, new BindingSetHandler() {
            @Override
            public void handle(BindingSet bindingSet) {
                totalReceivedHandshakesWithCommonTopics++;

                if (null != timeOfLastHandshake) {
                    long latency = System.currentTimeMillis() - timeOfLastHandshake;
                    timeOfLastHandshake = null;
                    System.out.println("handshake latency = " + latency + "ms");
                }

                System.out.println("GOT A 'TOPICS' HANDSHAKE: " + bindingSet);
            }
        });

        double averagePeopleKNown = (1 + MAX_PEOPLE_KNOWN) / 2;
        double averagePapersPerPerson = (1 + MAX_PAPERS) / 2;
        double averageTopicsPerPaper = (1 + MAX_TOPICS) / 2;
        double averageTopicsPerPerson = averagePapersPerPerson * averageTopicsPerPaper;
        int nTopics = (int) (averageTopicsPerPerson
                / (1 - Math.pow(1 - PROB_TOPICS_IN_COMMON, 1.0 / averageTopicsPerPerson)));

        places = new Place[TOTAL_PLACES];
        for (int i = 0; i < TOTAL_PLACES; i++) {
            places[i] = new Place(i);
        }

        topics = new Topic[nTopics];
        for (int i = 0; i < nTopics; i++) {
            topics[i] = new Topic(i);
        }

        int totalPapers = 0;
        int totalTopics = 0;
        int totalKnown = 0;

        long now = System.currentTimeMillis();
        people = new Person[TOTAL_PEOPLE];
        for (int i = 0; i < TOTAL_PEOPLE; i++) {
            Person person = new Person(i);
            people[i] = person;

            // every person has at least one paper and at most MAX_PAPERS
            int nPersonPapers = 1 + random.nextInt(MAX_PAPERS);
            for (int j = 0; j < nPersonPapers; j++) {
                Paper paper = new Paper(totalPapers++);
                person.papers.add(paper);

                // each paper has at least one topic and at most MAX_TOPICS
                int nPaperTopics = 1 + random.nextInt(MAX_TOPICS);
                for (int k = 0; k < nPaperTopics; k++) {
                    Topic topic = topics[random.nextInt(nTopics)];
                    paper.topics.add(topic);
                    totalTopics++;
                }
            }

            person.moveTo(randomPlace(), now);
        }
        for (Person person : people) {
            int nKnown = 1 + random.nextInt(MAX_PEOPLE_KNOWN);
            for (int j = 0; j < nKnown; j++) {
                Person other = people[random.nextInt(TOTAL_PEOPLE)];
                person.known.add(other);
            }
            totalKnown += nKnown;
        }

        System.out.println("total people: " + TOTAL_PEOPLE);
        System.out.println("total places: " + TOTAL_PLACES);
        System.out.println("total papers: " + totalPapers);
        System.out.println("total topics: " + nTopics);
        System.out.println("average papers per person (projected): " + averagePapersPerPerson);
        System.out.println("average papers per person (actual): " + (totalPapers / (1.0 * TOTAL_PEOPLE)));
        System.out.println("average topics per paper (projected): " + averageTopicsPerPaper);
        System.out.println("average topics per paper (actual): " + (totalTopics / (1.0 * totalPapers)));
        System.out.println("average topics per person (projected): " + averageTopicsPerPerson);
        System.out.println("average topics per person (actual): " + (totalTopics / (1.0 * TOTAL_PEOPLE)));
        System.out.println("average people known (projected): " + averagePeopleKNown);
        System.out.println("average people known (actual): " + (totalKnown / (1.0 * TOTAL_PEOPLE)));
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

        startTime = System.currentTimeMillis();

        long initialDelay = 0;
        ScheduledExecutorService service = Executors.newSingleThreadScheduledExecutor();
        service.scheduleWithFixedDelay(new Runnable() {
            public void run() {
                long now = System.currentTimeMillis();
                System.out.println("time step " + now);
                for (Person p : people) {
                    try {
                        p.considerMoving(now);
                        p.considerShakingHands();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
                long after = System.currentTimeMillis();
                if (after - now > TIME_STEP_MILLISECONDS) {
                    System.err.println("WARNING: processing took longer (" + (after - now)
                            + "ms) than time step (" + TIME_STEP_MILLISECONDS + "ms)");
                }

                long time = after - startTime;
                double movesPerSecond = totalMoves * 1000.0 / time;
                double halfshakesPerSecond = totalHalfshakes * 1000.0 / time;
                double shakesPerSecond = totalShakes * 1000.0 / time;
                double receivedHalfshakesPerSecond = (totalReceivedHalfshakePairs / 2) * 1000.0 / time;
                double receivedHandshakesWithCommonTopicsPerSecond
                        = (totalReceivedHandshakesWithCommonTopics / 2) * 1000.0 / time;
                double receivedHandshakesWithCommonKnowsPerSecond
                        = (totalReceivedHandshakesWithCommonKnows / 2) * 1000.0 / time;
                double movePeriod = TOTAL_PEOPLE * time / (totalMoves * 1000.0);
                double halfshakePeriod = TOTAL_PEOPLE * time / (totalHalfshakes * 1000.0);
                double shakePeriod = TOTAL_PEOPLE * time / (totalShakes * 1000.0);
                double receivedHalfshakePeriod = TOTAL_PEOPLE * time / ((totalReceivedHalfshakePairs / 2) * 1000.0);
                double receivedTruePositiveHalfshakePeriod = TOTAL_PEOPLE * time
                        / (totalReceivedTruePositiveHalfshakePairs * 1000.0);
                double receivedHandshakesWithCommonTopicsPeriod = TOTAL_PEOPLE * time
                        / ((totalReceivedHandshakesWithCommonTopics / 2) * 1000.0);
                double receivedHandshakesWithCommonKnowsPeriod = TOTAL_PEOPLE * time
                        / ((totalReceivedHandshakesWithCommonKnows / 2) * 1000.0);
                System.out.println("average moves per second: " + movesPerSecond);
                System.out.println("average half-shakes per second: " + halfshakesPerSecond);
                System.out.println("average shakes per second: " + shakesPerSecond);
                System.out.println("average received half-shakes per second: " + receivedHalfshakesPerSecond);
                System.out.println("average received handshakes with common topics per second: "
                        + receivedHandshakesWithCommonTopicsPerSecond);
                System.out.println("average received handshakes with common knows per second: "
                        + receivedHandshakesWithCommonKnowsPerSecond);
                System.out.println("average time between moves, per person: " + movePeriod);
                System.out.println("average time between half-shakes, per person: " + halfshakePeriod);
                System.out.println("average time between shakes, per person: " + shakePeriod);
                System.out.println("average time between received half-shake pairs, per person: "
                        + receivedHalfshakePeriod);
                System.out.println("average time between received true positive half-shake pairs, per person: "
                        + receivedTruePositiveHalfshakePeriod);
                System.out.println("average time between handshakes with common topics, per person: "
                        + receivedHandshakesWithCommonTopicsPeriod);
                System.out.println("average time between handshakes with common knows, per person: "
                        + receivedHandshakesWithCommonKnowsPeriod);
            }
        }, initialDelay, TIME_STEP_MILLISECONDS, TimeUnit.MILLISECONDS);

        /*
        service.scheduleWithFixedDelay(new Runnable() {
            public void run() {
                System.out.println("queryEngine: " + queryEngine);
                System.out.println("break here");
            }
        }, initialDelay, 60, TimeUnit.SECONDS);
        //*/
    }

    // star topology minimizes repeat handshakes
    private Place randomPlace() {
        return places[random.nextInt(places.length)];
    }

    public static void main(final String[] args) throws Exception {
        new SesameStreamEvaluation();
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

        timeOfLastHandshake = System.currentTimeMillis();
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

        timeOfLastPulse = System.currentTimeMillis();
        queryEngine.addStatements(person1.halfHandshakeTtl, toArray(d1));
        queryEngine.addStatements(person2.halfHandshakeTtl, toArray(d2));
    }

    private void presence(final Person person, final Place place) throws IOException {
        Statement st = vf.createStatement(
                person.uri, vf.createURI(ExtendoActivityOntology.NAMESPACE + "locatedAt"), place.uri);
        queryEngine.addStatements(PRESENCE_TTL, st);
    }

    private Statement[] toArray(final Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
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

        private Long timeOfLastMove;

        private Place currentPlace;

        private Person(final int id) {
            super(id);

            // note: avoids 0 as a value of TTL
            this.halfHandshakeTtl = 1 + random.nextInt(MAX_HALF_HANDSHAKE_TTL);
        }

        @Override
        protected String type() {
            return "person";
        }

        public void moveTo(final Place place, long now) throws IOException {
            System.out.println(this + " is moving to " + place
                    + (null == timeOfLastMove ? "" : (" after " + (now - timeOfLastMove) / 1000) + "s dwell time"));
            timeOfLastMove = now;

            if (null != currentPlace) {
                currentPlace.people.remove(this);
            }

            currentPlace = place;
            currentPlace.people.add(this);
        }

        public void considerMoving(long now) throws IOException {
            if (random.nextDouble() < PROB_MOVE) {
                Place newPlace;
                do {
                    newPlace = randomPlace();
                } while (newPlace == currentPlace);

                totalMoves++;
                moveTo(randomPlace(), now);
            }

            // refresh the person's current position, regardless of whether the person moved
            presence(this, currentPlace);
        }

        public void considerShakingHands() throws IOException {
            if (random.nextDouble() < PROB_HANDSHAKE) {
                if (currentPlace.people.size() > 0) {
                    // choose a single person in the newly entered space to shake hands with
                    Person other = currentPlace.people.iterator().next();

                    halfHandshakes(this, other);
                }
            }
        }
    }

    private class Place extends Thing {
        private final LinkedHashSet<Person> people = new LinkedHashSet<Person>();

        private Place(final int id) {
            super(id);
        }

        @Override
        protected String type() {
            return "place";
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
}
