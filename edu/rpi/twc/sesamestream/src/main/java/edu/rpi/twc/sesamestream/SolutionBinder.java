package edu.rpi.twc.sesamestream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface SolutionBinder {
    void bind(PartialSolution ps,
              TriplePattern p,
              VarList l);
}
