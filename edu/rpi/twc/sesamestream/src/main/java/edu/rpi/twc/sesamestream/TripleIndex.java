package edu.rpi.twc.sesamestream;

import org.openrdf.model.Statement;
import org.openrdf.model.Value;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TripleIndex {
    private TriplePattern pattern;
    private Map<Value, TripleIndex> boundVariableIndices;
    private TripleIndex unboundVariableTripleIndex;

    public void index(final VarList list,
                      final TriplePattern pattern) {
        if (list.isNil()) {
            this.pattern = pattern;
        } else {
            Value v = list.getValue();

            if (null == v) {
                if (null == unboundVariableTripleIndex) {
                    unboundVariableTripleIndex = new TripleIndex();
                }

                unboundVariableTripleIndex.index(list.getNext(), pattern);
            } else {
                if (null == boundVariableIndices) {
                    boundVariableIndices = new HashMap<Value, TripleIndex>();
                }

                TripleIndex n = boundVariableIndices.get(v);

                if (null == n) {
                    n = new TripleIndex();
                    boundVariableIndices.put(v, n);
                }

                n.index(list.getNext(), pattern);
            }
        }
    }

    public void match(final VarList list,
                      final Statement st,
                      final VarListHandler handler) {
        if (list.isNil()) {
            VarList bindings = VarList.NIL;

            if (!pattern.getSubject().hasValue()) {
                bindings = new VarList(pattern.getSubject().getName(), st.getSubject(), bindings);
            }

            if (!pattern.getPredicate().hasValue()) {
                bindings = new VarList(pattern.getPredicate().getName(), st.getPredicate(), bindings);
            }

            if (!pattern.getObject().hasValue()) {
                bindings = new VarList(pattern.getObject().getName(), st.getObject(), bindings);
            }

            handler.handle(bindings);
        } else {
            if (null != boundVariableIndices) {
                TripleIndex child = boundVariableIndices.get(list.getValue());

                if (null != child) {
                    child.match(list.getNext(), st, handler);
                }
            }

            if (null != unboundVariableTripleIndex) {
                unboundVariableTripleIndex.match(list.getNext(), st, handler);
            }
        }
    }
}
