package edu.rpi.twc.sesamestream.sim;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class QueryEngineSim {
    private final PriorityQueue<PartialSolutionSim> deletionQueue;

    private final Map<TriplePatternSim, List<PartialSolutionSim>> index;

    public QueryEngineSim() {
        deletionQueue = new PriorityQueue<PartialSolutionSim>();
        index = new HashMap<TriplePatternSim, List<PartialSolutionSim>>();
    }

    public void addQuery(final int size) {
        // a query never expires
        PartialSolutionSim ps = new PartialSolutionSim(size, 0);
        add(ps);
    }

    public void addStatement(final int window) {

    }

    private void add(final PartialSolutionSim ps) {
        for (TriplePatternSim tp : ps.graphPattern) {
            List<PartialSolutionSim> solutions = index.get(tp);
            if (null == solutions) {
                solutions = new LinkedList<PartialSolutionSim>();
                index.put(tp, solutions);
            }
            solutions.add(ps);
        }
    }

    private void remove(final PartialSolutionSim ps) {
        for (TriplePatternSim tp : ps.graphPattern) {
            List<PartialSolutionSim> solutions = index.get(tp);
            if (null != solutions) {
                solutions.remove(ps);
                if (0 == solutions.size()) {
                    index.remove(tp);
                }
            }
        }
    }

    private void cleanup() {

    }

    private class TriplePatternSim {

    }

    private class PartialSolutionSim implements Comparable<PartialSolutionSim> {
        private final long expirationTime;
        private final List<TriplePatternSim> graphPattern;

        public PartialSolutionSim(final int size,
                                  final long expirationTime) {
            graphPattern = new LinkedList<TriplePatternSim>();
            for (int i = 0; i < size; i++) {
                graphPattern.add(new TriplePatternSim());
            }
        }

        public int compareTo(final PartialSolutionSim other) {
            return ((Long) expirationTime).compareTo(other.expirationTime);
        }
    }


}
