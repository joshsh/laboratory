package edu.rpi.twc.sesamestream;

import org.openrdf.model.Statement;
import org.openrdf.model.Value;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TripleIndex {
    private static final int
            SUBJECT = 1,
            PREDICATE = 2,
            OBJECT = 3;

    private List<PartialSolution> partialSolutions;
    private Map<Value, TripleIndex> boundVariableIndices;
    private TripleIndex unboundVariableTripleIndex;

    public void index(final VarList list,
                      final PartialSolution ps) {
        if (list.isNil()) {
            //System.out.println("indexing nil list to " + ps);
            if (null == partialSolutions) {
                partialSolutions = new LinkedList<PartialSolution>();
            }
            partialSolutions.add(ps);
        } else {
            Value v = list.getValue();

            if (null == v) {
                //System.out.println("indexing null from list " + list + " to " + ps);
                if (null == unboundVariableTripleIndex) {
                    unboundVariableTripleIndex = new TripleIndex();
                }

                unboundVariableTripleIndex.index(list.getNext(), ps);
            } else {
                //System.out.println("indexing value " + v + " from list " + list + " to " + ps);

                if (null == boundVariableIndices) {
                    boundVariableIndices = new HashMap<Value, TripleIndex>();
                }

                TripleIndex n = boundVariableIndices.get(v);

                if (null == n) {
                    n = new TripleIndex();
                    boundVariableIndices.put(v, n);
                }

                n.index(list.getNext(), ps);
            }
        }
    }

    public void match(final VarList list,
                      final Statement st,
                      final SolutionBinder handler) {
        if (list.isNil()) {
            //System.out.println("matching nil list for statement " + st);
            if (null != partialSolutions) {
                VarList bindings = VarList.NIL;

                for (PartialSolution ps : partialSolutions) {
                    for (TriplePattern p : ps.getPatterns()) {
                        if (!p.getSubject().hasValue()) {
                            bindings = new VarList(p.getSubject().getName(), st.getSubject(), bindings);
                        }

                        if (!p.getPredicate().hasValue()) {
                            bindings = new VarList(p.getPredicate().getName(), st.getPredicate(), bindings);
                        }

                        if (!p.getObject().hasValue()) {
                            bindings = new VarList(p.getObject().getName(), st.getObject(), bindings);
                        }

                        handler.bind(ps, p, bindings);
                    }
                }
            }
        } else {
            //System.out.println("matching non-nil list " + list + " for statement: " + st);
            if (null != boundVariableIndices) {
                //System.out.println("we have bound variable indices");
                TripleIndex child = boundVariableIndices.get(list.getValue());

                if (null != child) {
                    //System.out.println("child found");
                    child.match(list.getNext(), st, handler);
                }
            }

            if (null != unboundVariableTripleIndex) {
                //System.out.println("we have unbound variable indices");
                unboundVariableTripleIndex.match(list.getNext(), st, handler);
            }
        }
    }
}
