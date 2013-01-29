package edu.rpi.twc.sesamestream;

import org.openrdf.model.Value;
import org.openrdf.query.algebra.Var;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class VarList {

    public static final VarList NIL = new VarList();

    private final String name;
    private final Value value;
    private final VarList next;

    private VarList() {
        name = null;
        value = null;
        next = null;
    }

    public VarList(String name, Value value, VarList next) {
        this.name = name;
        this.value = value;
        this.next = next;
    }

    public boolean isNil() {
        return next == null;
    }

    public String getName() {
        return name;
    }

    public Value getValue() {
        return value;
    }

    public VarList getNext() {
        return next;
    }

    public Var asVar() {
        return new Var(name, value);
    }

    public VarList prepend(final VarList other) {
        VarList cur1 = this;
        VarList cur2 = other;

        while (!cur2.isNil()) {
            cur1 = new VarList(cur2.name, cur2.value, cur1);
            cur2 = cur2.next;
        }

        return cur1;
    }

    public int length() {
        VarList cur = this;
        int l = 0;
        while (cur != null) {
            l++;
            cur = cur.next;
        }

        return l;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("VarList(");
        boolean first = true;
        VarList cur = this;
        while (!cur.isNil()) {
            if (first) {
                first = false;
            } else {
                sb.append(",");
            }

            sb.append(cur.name).append(":").append(cur.value);

            cur = cur.next;
        }
        sb.append(")");
        return sb.toString();
    }
}
