package com.franz.benchmarking.sp2bench;

import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.cypher.javacompat.ExecutionResult;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import org.neo4j.kernel.impl.util.StringLogger;

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

    private static void timeQuery(final String query,
                                  final int iters,
                                  final int sets,
                                  final ExecutionEngine engine,
                                  final List<Long> counts,
                                  final List<Long> times) {
        for (int j = 0; j < sets; j++) {
            long time1 = System.currentTimeMillis();

            long count = 0;
            for (int i = 0; i < iters; i++) {
                ExecutionResult result = engine.execute(query);
                // TODO: restore iteration in AG and Neo test loops
                for (Map<String, Object> row : result) {
                    count++;
                }
            }

            long time2 = System.currentTimeMillis();

            counts.add(count);
            times.add(time2 - time1);
        }
    }

    private static void printTimedQuery(final String queryName,
                                        final String query,
                                        final int iters,
                                        final int sets,
                                        final ExecutionEngine engine) {
        List<Long> counts = new LinkedList<Long>();
        List<Long> times = new LinkedList<Long>();

        timeQuery(query, iters, sets, engine, counts, times);

        StringBuilder sb = new StringBuilder();
        sb.append(queryName);
        sb.append(".neo4j <- c(");
        boolean first = true;
        for (long time : times) {
            if (first) first = false; else sb.append(", ");

            sb.append(time);
        }
        sb.append(")");

        System.out.println(sb);

        sb = new StringBuilder();
        sb.append(queryName);
        sb.append(".neo4j.count <- c(");
        first = true;
        for (long count : counts) {
            if (first) first = false; else sb.append(", ");

            sb.append(count);
        }
        sb.append(")");

        System.out.println(sb);
    }

    public static void main(final String[] args) throws Exception {
        /*
SELECT ?yr
WHERE {
  ?journal rdf:type bench:Journal .
  ?journal dc:title "Journal 1 (1940)"^^xsd:string .
  ?journal dcterms:issued ?yr
}
         */

        GraphDatabaseFactory factory = new GraphDatabaseFactory();
        GraphDatabaseService g = factory.newEmbeddedDatabase("/tmp/neo-sp2bench");
        try {
            // Return the year of publication of Journal 1 (1940).
            String q1 = "START journal=node(*)\n" +
                    "MATCH journal-[:`rdf:type`]->bj, journal-[:`dc:title`]->title, journal-[:`dcterms:issued`]->year\n" +
                    "WHERE bj.__id = 'bench:Journal' AND title.__id = '\"Journal 1 (1940)\"'\n" +
                    "RETURN year.__id";

            // Extract all inproceedings with properties
            // dc:creator, bench:booktitle, dc:title, swrc:pages, dcterms:partOf, rdfs:seeAlso, foaf:homepage
            // dcterms:issued, and optionally bench:abstract, including these properties, ordered by year.
            String q2 = "START inproc=node(*)\n" +
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

            // Select all articles with property swrc:pages.
            String q3a = "START article=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:pages`]->value\n" +
                    "WHERE Art.__id = 'bench:Article'\n" +
                    "RETURN article.__id";

            // Select all articles with property swrc:month.
            String q3b = "START article=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:month`]->value\n" +
                    "WHERE Art.__id = 'bench:Article'\n" +
                    "RETURN article.__id";

            // Select all articles with property swrc:isbn.
            String q3c = "START article=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:isbn`]->value\n" +
                    "WHERE Art.__id = 'bench:Article'\n" +
                    "RETURN article.__id";

            // Select all distinct pairs of article author names for authors that have published in the same journal.
            // TODO: very poor performance (perhaps never terminating)
            String q4 = "START article1=node(*), article2=node(*)\n" +
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

            // Return the names of all persons that occur as author of at least one inproceeding and at least one article.
            // Note: queries q5 and q5a are treated as the same query, as they differ only in FILTER syntax
            String q5 = "START person=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art," +
                    " article-[:`dc:creator`]->person," +
                    " inproc-[:`rdf:type`]->Inproc," +
                    " inproc-[:`dc:creator`]->person," +
                    " person-[:`foaf:name`]->name\n" +
                    "WHERE Art.__id = 'bench:Article' AND Inproc.__id = 'bench:Inproceedings'\n" +
                    "RETURN DISTINCT person.__id, name.__id";

            ExecutionEngine engine = new ExecutionEngine(g, StringLogger.SYSTEM);
            Transaction tx = g.beginTx();

            //showQueryResult(q1, engine);

            printTimedQuery("q3c", q3c, 1, 10, engine);

        } finally {
            g.shutdown();
        }

    }
}
