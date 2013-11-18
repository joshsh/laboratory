package com.franz.benchmarking.sp2bench;

import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.util.wrappers.id.IdGraph;
import com.tinkerpop.gremlin.groovy.Gremlin;
import com.tinkerpop.pipes.Pipe;
import com.tinkerpop.pipes.util.iterators.SingleIterator;
import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.cypher.javacompat.ExecutionResult;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import org.neo4j.kernel.impl.util.StringLogger;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SP2BenchQueries {

    private static void showQueryResult(final String query,
                                        final ExecutionEngine engine) {
        long time1 = System.currentTimeMillis();
        ExecutionResult result = engine.execute(query);
        long time2 = System.currentTimeMillis();

        String rows = "";
        for (Map<String, Object> row : result) {
            for (Map.Entry<String, Object> column : row.entrySet()) {
                rows += column.getKey() + ": " + column.getValue() + "; ";
            }
            rows += "\n";
        }
        System.out.print(rows);
        long time3 = System.currentTimeMillis();

        System.out.println("execution time: " + (time2 - time1) + "ms");
        System.out.println("iteration time: " + (time3 - time2) + "ms");
    }

    private static void timeCypherQuery(final Query query,
                                        final int iters,
                                        final int sets,
                                        final List<Long> counts,
                                        final List<Long> times) {
        for (int j = 0; j < sets; j++) {
            long time1 = System.currentTimeMillis();

            long count = query.execute(iters);

            long time2 = System.currentTimeMillis();

            counts.add(count);
            times.add(time2 - time1);
        }
    }

    private static void evalQuery(final Query query,
                                  final int iters,
                                  final int sets) {
        List<Long> counts = new LinkedList<Long>();
        List<Long> times = new LinkedList<Long>();

        timeCypherQuery(query, iters, sets, counts, times);

        StringBuilder sb = new StringBuilder();
        sb.append(query.getName());
        sb.append(" <- c(");
        boolean first = true;
        for (long time : times) {
            if (first) first = false;
            else sb.append(", ");

            sb.append(time);
        }
        sb.append(")");

        System.out.println(sb);

        sb = new StringBuilder();
        sb.append(query.getName());
        sb.append(".count <- c(");
        first = true;
        for (long count : counts) {
            if (first) first = false;
            else sb.append(", ");

            sb.append(count);
        }
        sb.append(")");

        System.out.println(sb);
    }

    public static void main(final String[] args) throws Exception {

        String pathToDatabase = "/tmp/sp2bench-neo/50k";

        new SP2BenchQueries().evaluateCypher(pathToDatabase);
        //new SP2BenchQueries().evaluateGremlin(pathToDatabase);
    }

    private void evaluateCypher(final String pathToDatabase) {
        GraphDatabaseFactory factory = new GraphDatabaseFactory();
        GraphDatabaseService g = factory.newEmbeddedDatabase(pathToDatabase);
        try {
            // Return the year of publication of Journal 1 (1940).
            String q1_cypher = "START journal=node(*)\n" +
                    "MATCH journal-[:`rdf:type`]->bj, journal-[:`dc:title`]->title, journal-[:`dcterms:issued`]->year\n" +
                    "WHERE bj.__id = 'bench:Journal' AND title.__id = '\"Journal 1 (1940)\"'\n" +
                    "RETURN year.__id";
            String q1_cypher_cheat1 = "START title=node:node_auto_index(__id = \"\\\"Journal 1 (1940)\\\"\")\n" +
                    "MATCH journal-[:`rdf:type`]->bj, journal-[:`dc:title`]->title, journal-[:`dcterms:issued`]->year\n" +
                    "WHERE bj.__id = 'bench:Journal'\n" +
                    "RETURN year.__id";

            // Extract all inproceedings with properties
            // dc:creator, bench:booktitle, dc:title, swrc:pages, dcterms:partOf, rdfs:seeAlso, foaf:homepage
            // dcterms:issued, and optionally bench:abstract, including these properties, ordered by year.
            String q2_cypher = "START inproc=node(*)\n" +
                    "MATCH inproc-[:`rdf:type`]->bip," +
                    " inproc-[:`dc:creator`]->author," +
                    " inproc-[:`bench:booktitle`]->booktitle," +
                    " inproc-[:`dc:title`]->title," +
                    " inproc-[:`dcterms:partOf`]->proc," +
                    " inproc-[:`rdfs:seeAlso`]->ee," +
                    " inproc-[:`swrc:pages`]->page," +
                    " inproc-[:`foaf:homepage`]->url," +
                    " inproc-[:`dcterms:issued`]->yr," +
                    " inproc-[?:`bench:abstract`]->abstract\n" +
                    "WHERE bip.__id = 'bench:Inproceedings'\n" +
                    "RETURN inproc.__id, author.__id, booktitle.__id, title.__id, proc.__id, ee.__id, page.__id, url.__id, yr.__id, abstract.__id";
            String q2_cypher_cheat = "START bip=node:node_auto_index(__id = \"bench:Inproceedings\")\n" +
                    "MATCH inproc-[:`rdf:type`]->bip," +
                    " inproc-[:`dc:creator`]->author," +
                    " inproc-[:`bench:booktitle`]->booktitle," +
                    " inproc-[:`dc:title`]->title," +
                    " inproc-[:`dcterms:partOf`]->proc," +
                    " inproc-[:`rdfs:seeAlso`]->ee," +
                    " inproc-[:`swrc:pages`]->page," +
                    " inproc-[:`foaf:homepage`]->url," +
                    " inproc-[:`dcterms:issued`]->yr," +
                    " inproc-[?:`bench:abstract`]->abstract\n" +
                    "RETURN inproc.__id, author.__id, booktitle.__id, title.__id, proc.__id, ee.__id, page.__id, url.__id, yr.__id, abstract.__id";

            // Select all articles with property swrc:pages.
            String q3a_cypher = "START article=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:pages`]->value\n" +
                    "WHERE Art.__id = 'bench:Article'\n" +
                    "RETURN article.__id";
            String q3a_cypher_cheat = "START Art=node:node_auto_index(__id = \"bench:Article\")\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:pages`]->value\n" +
                    "RETURN article.__id";

            // Select all articles with property swrc:month.
            String q3b_cypher = "START article=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:month`]->value\n" +
                    "WHERE Art.__id = 'bench:Article'\n" +
                    "RETURN article.__id";
            String q3b_cypher_cheat = "START Art=node:node_auto_index(__id = \"bench:Article\")\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:month`]->value\n" +
                    "RETURN article.__id";

            // Select all articles with property swrc:isbn.
            String q3c_cypher = "START article=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:isbn`]->value\n" +
                    "WHERE Art.__id = 'bench:Article'\n" +
                    "RETURN article.__id";
            String q3c_cypher_cheat = "START Art=node:node_auto_index(__id = \"bench:Article\")\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:isbn`]->value\n" +
                    "RETURN article.__id";

            // Select all distinct pairs of article author names for authors that have published in the same journal.
            // TODO: very poor performance (perhaps never terminating)
            String q4_cypher = "START article1=node(*), article2=node(*)\n" +
                    "MATCH article1-[:`rdf:type`]->Art," +
                    " article2-[:`rdf:type`]->Art," +
                    " article1-[:`dc:creator`]->author1," +
                    " author1-[:`foaf:name`]->name1," +
                    " article2-[:`dc:creator`]->author2," +
                    " author2-[:`foaf:name`]->name2," +
                    " article1-[:`swrc:journal`]->journal1," +
                    " article2-[:`swrc:journal`]->journal2\n" +
                    "WHERE Art.__id = 'bench:Article' AND name1.__id < name2.__id\n" +
                    "RETURN DISTINCT name1.__id, name2.__id";
            String q4_cypher_cheat = "START Art=node:node_auto_index(__id = \"bench:Article\")\n" +
                    "MATCH article1-[:`rdf:type`]->Art," +
                    " article2-[:`rdf:type`]->Art," +
                    " article1-[:`dc:creator`]->author1," +
                    " author1-[:`foaf:name`]->name1," +
                    " article2-[:`dc:creator`]->author2," +
                    " author2-[:`foaf:name`]->name2," +
                    " article1-[:`swrc:journal`]->journal1," +
                    " article2-[:`swrc:journal`]->journal2\n" +
                    "WHERE name1.__id < name2.__id\n" +
                    "RETURN DISTINCT name1.__id, name2.__id";

            // Return the names of all persons that occur as author of at least one inproceeding and at least one article.
            // Note: queries q5 and q5a are treated as the same query, as they differ only in FILTER syntax
            String q5_cypher = "START person=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art," +
                    " article-[:`dc:creator`]->person," +
                    " inproc-[:`rdf:type`]->Inproc," +
                    " inproc-[:`dc:creator`]->person," +
                    " person-[:`foaf:name`]->name\n" +
                    "WHERE Art.__id = 'bench:Article' AND Inproc.__id = 'bench:Inproceedings'\n" +
                    "RETURN DISTINCT person.__id, name.__id";
            String q5_cypher_cheat1 = "START Art=node:node_auto_index(__id = \"bench:Article\")\n" +
                    "MATCH article-[:`rdf:type`]->Art," +
                    " article-[:`dc:creator`]->person," +
                    " inproc-[:`rdf:type`]->Inproc," +
                    " inproc-[:`dc:creator`]->person," +
                    " person-[:`foaf:name`]->name\n" +
                    "WHERE Inproc.__id = 'bench:Inproceedings'\n" +
                    "RETURN DISTINCT person.__id, name.__id";

            /*
            // Return, for each year, the set of all publications authored by persons that have not published in years before.
            String q6_cypher = "START class=node(*)\n" +
                    "MATCH class-[:`rdfs:subClassOf`]->Doc," +
                    " document-[:`rdf:type`]->class," +
                    " document-[:`dcterms:issued`]->yr," +
                    " document-[:`dc:creator`]->author," +
                    " author-[:`foaf:name`]->name\n" +
                    "WHERE Doc.__id = 'foaf:Document' AND author.__id = author2.__id AND yr2.__id < yr.__id " +
                    " AND NOT(class2-[:`rdfs:subClassOf`]->Doc," +
                    " document2-[:`rdf:type`]->class2," +
                    " document2-[:`dcterms:issued`]->yr2," +
                    " document2-[:`dc:creator`]->author2)\n" +
                    "RETURN yr.__id, name.__id, document.__id";
            String q6_cypher_cheat = "";

            String q7_cypher = "START class=node(*)\n" +
                    "MATCH class-[:`rdfs:subClassOf`]->Doc," +
                    " doc-[:`rdf:type`]->class,\n" +
                    " doc-[:`dc:title`]->title,\n" +
                    " bag2-[member2]->doc,\n" +
                    " doc2-[:`dcterms:references`]->bag2,\n" +
                    //optional...
                    " bag3-[member3]->doc,\n" +
                    //...
                    "WHERE Doc.__id = 'foaf:Document'\n" +
                    "RETURN DISTINCT title.__id";
            String q7_cypher_cheat = "";
            */

            String q8_cypher_part1 = "START erdoes=node(*)\n" +
                    "MATCH erdoes-[:`rdf:type`]->Person," +
                    " erdoes-[:`foaf:name`]->paulname," +
                    " document-[:`dc:creator`]->erdoes," +
                    " document-[:`dc:creator`]->author," +
                    " document2-[:`dc:creator`]->author," +
                    " document2-[:`dc:creator`]->author2," +
                    " author2-[:`foaf:name`]->name\n" +
                    "WHERE Person.__id = 'foaf:Person' AND paulname.__id = \"\\\"Paul Erdoes\\\"\"" + // note: xsd:string datatype constraint is ignored
                    " AND author <> erdoes AND document2 <> document AND author2 <> erdoes AND author2 <> author\n" +
                    "RETURN DISTINCT name.__id";
            String q8_cypher_part2 = "START erdoes=node(*)\n" +
                    "MATCH erdoes-[:`rdf:type`]->Person," +
                    " erdoes-[:`foaf:name`]->paulname," +
                    " document-[:`dc:creator`]->erdoes," +
                    " document-[:`dc:creator`]->author," +
                    " author-[:`foaf:name`]->name\n" +
                    "WHERE Person.__id = 'foaf:Person' AND paulname.__id = \"\\\"Paul Erdoes\\\"\"" + // note: xsd:string datatype constraint is ignored
                    " AND author <> erdoes\n" +
                    "RETURN DISTINCT name.__id";
            String q8_cypher_cheat1_part1 = "START paulname=node:node_auto_index(__id = \"\\\"Paul Erdoes\\\"\")\n" +
                    "MATCH erdoes-[:`rdf:type`]->Person," +
                    " erdoes-[:`foaf:name`]->paulname," +
                    " document-[:`dc:creator`]->erdoes," +
                    " document-[:`dc:creator`]->author," +
                    " document2-[:`dc:creator`]->author," +
                    " document2-[:`dc:creator`]->author2," +
                    " author2-[:`foaf:name`]->name\n" +
                    "WHERE Person.__id = 'foaf:Person'" +
                    " AND author <> erdoes AND document2 <> document AND author2 <> erdoes AND author2 <> author\n" +
                    "RETURN DISTINCT name.__id";
            String q8_cypher_cheat1_part2 = "START paulname=node:node_auto_index(__id = \"\\\"Paul Erdoes\\\"\")\n" +
                    "MATCH erdoes-[:`rdf:type`]->Person," +
                    " erdoes-[:`foaf:name`]->paulname," +
                    " document-[:`dc:creator`]->erdoes," +
                    " document-[:`dc:creator`]->author," +
                    " author-[:`foaf:name`]->name\n" +
                    "WHERE Person.__id = 'foaf:Person'" +
                    " AND author <> erdoes\n" +
                    "RETURN DISTINCT name.__id";

            String q9_cypher_part1 = "START person=node(*)\n" +
                    "MATCH person-[:`rdf:type`]->Person,\n" +
                    " subject-[predicate]->person\n" +
                    "WHERE Person.__id = 'foaf:Person'\n" +
                    "RETURN DISTINCT predicate";
            String q9_cypher_part2 = "START person=node(*)\n" +
                    "MATCH person-[:`rdf:type`]->Person,\n" +
                    " person-[predicate]->object\n" +
                    "WHERE Person.__id = 'foaf:Person'\n" +
                    "RETURN DISTINCT predicate";
            String q9_cypher_cheat_part1 = "START Person=node:node_auto_index(__id = \"foaf:Person\")\n" +
                    "MATCH person-[:`rdf:type`]->Person,\n" +
                    " subject-[predicate]->person\n" +
                    "RETURN DISTINCT predicate";
            String q9_cypher_cheat_part2 = "START Person=node:node_auto_index(__id = \"foaf:Person\")\n" +
                    "MATCH person-[:`rdf:type`]->Person,\n" +
                    " person-[predicate]->object\n" +
                    "RETURN DISTINCT predicate";

            // Return all subjects that stand in any relation to Paul Erdoes.
            // In our scenario, the query might also be formulated as "Return publications and venues in which
            // Paul Erdoes is involved either as author or as editor".
            String q10_cypher = "START subject=node(*)\n" +
                    "MATCH subject-[predicate]->paul\n" +
                    "WHERE paul.__id = 'person:Paul_Erdoes'\n" +
                    "RETURN subject.__id, predicate";
            String q10_cypher_cheat = "START paul=node:node_auto_index(__id = \"person:Paul_Erdoes\")\n" +
                    "MATCH paul<-[predicate]-subject\n" +
                    "RETURN subject.__id, predicate";

            // TODO: try a q11 cheat based on an rdfs:seeAlso label lookup?  Can labels be indexed?
            // Return (up to) 10 electronic edition URLs starting from the 51th publication, in lexicographical order.
            String q11_cypher = "START publication=node(*)\n" +
                    "MATCH publication-[:`rdfs:seeAlso`]->ee\n" +
                    "RETURN ee.__id\n" +
                    "ORDER BY ee.__id\n" +
                    "SKIP 50\n" +
                    "LIMIT 10";

            // (a) Return yes if a person occurs as author of at least one inproceeding and article, no otherwise.
            // This query is the boolean counterpart of Q5a.
            String q12a_cypher = q5_cypher;
            String q12a_cypher_cheat1 = q5_cypher_cheat1;

            String q12b_cypher_part1 = q8_cypher_part1;
            String q12b_cypher_part2 = q8_cypher_part2;
            String q12b_cypher_cheat1_part1 = q8_cypher_cheat1_part1;
            String q12b_cypher_cheat1_part2 = q8_cypher_cheat1_part2;

            String q12c_cypher = "START john=node(*)\n" +
                    "MATCH john-[:`rdf:type`]->Person\n" +
                    "WHERE john.__id = 'person:John_Q_Public' AND Person.__id = 'foaf:Person'\n" +
                    "RETURN john.__id";
            String q12c_cypher_cheat1 = "START john=node:node_auto_index(__id = \"person:John_Q_Public\")\n" +
                    "MATCH john-[:`rdf:type`]->Person\n" +
                    "WHERE Person.__id = 'foaf:Person'\n" +
                    "RETURN john.__id";

            ExecutionEngine engine = new ExecutionEngine(g, StringLogger.SYSTEM);
            Transaction tx = g.beginTx();

            //showQueryResult(q11_cypher, engine);

            /*
            evalQuery(new CypherSelectQuery("q1.cypher", q1_cypher, engine), 1, 10);
            evalQuery(new CypherSelectQuery("q1.cypher.cheat1", q1_cypher_cheat1, engine), 1, 10);

            evalQuery(new CypherSelectQuery("q2.cypher", q2_cypher, engine), 1, 10);
            evalQuery(new CypherSelectQuery("q2.cypher.cheat", q2_cypher_cheat, engine), 1, 10);

            evalQuery(new CypherSelectQuery("q3a.cypher", q3a_cypher, engine), 1, 10);
            evalQuery(new CypherSelectQuery("q3a.cypher.cheat", q3a_cypher_cheat, engine), 1, 10);

            evalQuery(new CypherSelectQuery("q3b.cypher", q3b_cypher, engine), 1, 10);
            evalQuery(new CypherSelectQuery("q3b.cypher.cheat", q3b_cypher_cheat, engine), 1, 10);

            evalQuery(new CypherSelectQuery("q3c.cypher", q3c_cypher, engine), 1, 10);
            evalQuery(new CypherSelectQuery("q3c.cypher.cheat", q3c_cypher_cheat, engine), 1, 10);

            //evalQuery(new CypherSelectQuery("q4.cypher", q4_cypher, engine), 1, 10);
            //evalQuery(new CypherSelectQuery("q4.cypher.cheat", q4_cypher_cheat, engine), 1, 10);

            evalQuery(new CypherSelectQuery("q5.cypher", q5_cypher, engine), 1, 10);
            evalQuery(new CypherSelectQuery("q5.cypher.cheat1", q5_cypher_cheat1, engine), 1, 10);

            // q6 and q7 are inexpressible as Cypher queries
            //evalQuery(new CypherSelectQuery("q6.cypher", q6_cypher, engine), 1, 10);
            //evalQuery(new CypherSelectQuery("q7.cypher", q7_cypher, engine), 1, 10);

            //*/

            /*
            //evalQuery(new CypherSelectQuery("q8.cypher.part1", q8_cypher_part1, engine), 1, 10);
            //evalQuery(new CypherSelectQuery("q8.cypher.part2", q8_cypher_part2, engine), 1, 10);
            evalQuery(new CypherSelectUnionQuery("q8.cypher.union", q8_cypher_part1, q8_cypher_part2, engine), 1, 10);
            //evalQuery(new CypherSelectQuery("q8.cypher.cheat1.part1", q8_cypher_cheat1_part1, engine), 1, 10);
            //evalQuery(new CypherSelectQuery("q8.cypher.cheat1.part2", q8_cypher_cheat1_part2, engine), 1, 10);
            evalQuery(new CypherSelectUnionQuery("q8.cypher.cheat1.union", q8_cypher_cheat1_part1, q8_cypher_cheat1_part2, engine), 1, 10);

            evalQuery(new CypherSelectUnionQuery("q9.cypher.union", q9_cypher_part1, q9_cypher_part2, engine), 1, 10);
            evalQuery(new CypherSelectUnionQuery("q9.cypher.cheat.union", q9_cypher_cheat_part1, q9_cypher_cheat_part2, engine), 1, 10);

            //*/
            /*

            evalQuery(new CypherSelectQuery("q10.cypher", q10_cypher, engine), 1, 10);
            evalQuery(new CypherSelectQuery("q10.cypher.cheat", q10_cypher_cheat, engine), 1, 10);

            evalQuery(new CypherSelectQuery("q11.cypher", q11_cypher, engine), 1, 10);
            */

            evalQuery(new CypherAskQuery("q12a.cypher", q12a_cypher, engine), 1, 10);
            evalQuery(new CypherAskQuery("q12a.cypher.cheat1", q12a_cypher_cheat1, engine), 1, 10);

            evalQuery(new CypherAskUnionQuery("q12b.cypher.union", q12b_cypher_part1, q12b_cypher_part2, engine), 1, 10);
            evalQuery(new CypherAskUnionQuery("q12b.cypher.cheat1.union", q12b_cypher_cheat1_part1, q12b_cypher_cheat1_part2, engine), 1, 10);

            evalQuery(new CypherAskQuery("q12c.cypher", q12c_cypher, engine), 1, 10);
            evalQuery(new CypherAskQuery("q12c.cypher.cheat1", q12c_cypher_cheat1, engine), 1, 10);
            //*/
        } finally {
            g.shutdown();
        }
    }


    private void evaluateGremlin(final String pathToDatabase) {
        Neo4jGraph g = new Neo4jGraph(pathToDatabase);

        try {
            // TODO: necessary?
            g.getRawGraph().index().getNodeAutoIndexer().startAutoIndexingProperty(IdGraph.ID);

            IdGraph ig = new IdGraph(g, true, false);

            String q1_gremlin_root = "\"Journal 1 (1940)\"";
            String q1_gremlin = "_().in(\"dc:title\").as(\"journal\").out(\"rdf:type\").filter{it.id.equals(\"bench:Journal\")}.back(\"journal\")out(\"dcterms:issued\")";

            Query q = new GremlinQuery("q1.grm", q1_gremlin_root, q1_gremlin, ig);

            evalQuery(q, 1, 10);
        } finally {
            g.shutdown();
        }
    }

    private String getUnionKey(final ExecutionResult result) {
        List<String> columns = result.columns();
        if (1 != columns.size()) {
            throw new IllegalStateException("unexpected number of columns in union query result: " + columns.size());
        } else {
            return columns.iterator().next();
        }
    }

    private abstract class Query {
        private final String name;

        protected Query(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public abstract long execute(int iters);
    }

    private class CypherSelectQuery extends Query {
        private final String queryString;
        private final ExecutionEngine engine;

        public CypherSelectQuery(final String name,
                                 final String queryString,
                                 final ExecutionEngine engine) {
            super(name);
            this.engine = engine;
            this.queryString = queryString;
        }

        public long execute(final int iters) {
            // Note: if there are multiple iterations, only the last count is used.  All counts should be identical.
            // If for some reason there are no iterations, a count of 0 is used
            long count = 0;

            for (int i = 0; i < iters; i++) {
                count = 0;
                ExecutionResult result = engine.execute(queryString);

                for (Map<String, Object> row : result) {
                    count++;
                }
            }

            return count;
        }
    }

    private class CypherSelectUnionQuery extends Query {
        private final String queryString1;
        private final String queryString2;
        private final ExecutionEngine engine;

        public CypherSelectUnionQuery(final String name,
                                      final String queryString1,
                                      final String queryString2,
                                      final ExecutionEngine engine) {
            super(name);
            this.engine = engine;
            this.queryString1 = queryString1;
            this.queryString2 = queryString2;
        }

        public long execute(final int iters) {
            // Note: if there are multiple iterations, only the last count is used.  All counts should be identical.
            // If for some reason there are no iterations, a count of 0 is used
            long count = 0;

            for (int i = 0; i < iters; i++) {
                Collection<Object> c = new HashSet<Object>();

                ExecutionResult result = engine.execute(queryString1);
                String unionKey = getUnionKey(result);
                for (Map<String, Object> row : result) {
                    c.add(row.get(unionKey));
                }

                result = engine.execute(queryString2);
                unionKey = getUnionKey(result);
                for (Map<String, Object> row : result) {
                    c.add(row.get(unionKey));
                }

                count = c.size();
            }

            return count;
        }
    }

    private class CypherAskQuery extends Query {
        private final String queryString;
        private final ExecutionEngine engine;

        public CypherAskQuery(final String name,
                              final String queryString,
                              final ExecutionEngine engine) {
            super(name);
            this.engine = engine;
            this.queryString = queryString;
        }

        public long execute(final int iters) {
            // Note: if there are multiple iterations, only the last count is used.  All counts should be identical.
            // If for some reason there are no iterations, a count of 0 is used
            long count = 0;

            for (int i = 0; i < iters; i++) {
                ExecutionResult result = engine.execute(queryString);

                count = result.iterator().hasNext() ? 1 : 0;
            }

            return count;
        }
    }

    private class CypherAskUnionQuery extends Query {
        private final String queryString1;
        private final String queryString2;
        private final ExecutionEngine engine;

        public CypherAskUnionQuery(final String name,
                                   final String queryString1,
                                   final String queryString2,
                                   final ExecutionEngine engine) {
            super(name);
            this.engine = engine;
            this.queryString1 = queryString1;
            this.queryString2 = queryString2;
        }

        public long execute(final int iters) {
            // Note: if there are multiple iterations, only the last count is used.  All counts should be identical.
            // If for some reason there are no iterations, a count of 0 is used
            long count = 0;

            for (int i = 0; i < iters; i++) {
                count = 0;

                ExecutionResult result = engine.execute(queryString1);
                if (result.iterator().hasNext()) {
                    count = 1;
                    continue;
                }

                result = engine.execute(queryString2);
                if (result.iterator().hasNext()) {
                    count = 1;
                }
            }

            return count;
        }
    }

    private class GremlinQuery extends Query {
        private final Pipe pipe;
        private final IdGraph ig;
        private final String rootId;

        private GremlinQuery(final String name,
                             final String rootId,
                             final String queryString,
                             final IdGraph ig) {
            super(name);
            this.ig = ig;
            this.rootId = rootId;

            pipe = Gremlin.compile(queryString);
        }

        public long execute(final int iters) {
            long count = 0;

            for (int i = 0; i < iters; i++) {
                count = 0;

                Vertex root = ig.getVertex(rootId);
                if (null == root) {
                    throw new IllegalStateException("root vertex not found: " + rootId);
                }

                pipe.setStarts(new SingleIterator<Vertex>(root));
                for (Object r : pipe) {
                    count++;
                }
            }

            return count;
        }
    }
}
