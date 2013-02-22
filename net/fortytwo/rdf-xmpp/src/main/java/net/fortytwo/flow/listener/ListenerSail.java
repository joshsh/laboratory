/* Copyright (C) 2008 Knowledge Reef Systems.  All rights reserved. */

package com.knowledgereefsystems.agsail.sesameutils.listener;

import com.knowledgereefsystems.reef.onto.KRSException;
import net.fortytwo.ripple.flow.diff.DiffSink;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailChangedListener;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;
import org.openrdf.sail.StackableSail;
import org.openrdf.sail.helpers.SailWrapper;

import java.io.File;

/**
 * A Sail which listens on statements added to or removed from a base Sail.
 * It optionally reports those statements only when a change is committed, thus
 * omitting statements which are programmatically added to or removed from the
 * base Sail, but don't actually affect it.  Some Sail implementations, for
 * instance MemoryStore, report statements to their listeners immediately, even
 * if the change in question has not been committed.  This could lead to an
 * inaccurate log of changes.
 * <p/>
 * Author: josh
 * Date: Mar 7, 2008
 * Time: 1:03:14 PM
 */
public class ListenerSail extends SailWrapper {
    private DiffSink<Statement, KRSException> diffSink;
    private boolean waitForCommit;

    /**
     * Constructor.
     *
     * @param baseSail      the Sail to listen on
     * @param diffSink      a sink to which to report added or removed statements
     * @param waitForCommit if true, statements will only be reported on commit()
     *                      time, and will be forgotten on rollback().
     */
    public ListenerSail(final Sail baseSail, final DiffSink<Statement, KRSException> diffSink, final boolean waitForCommit) {
        super(baseSail);
        this.diffSink = diffSink;
        this.waitForCommit = waitForCommit;
    }

    public SailConnection getConnection() throws SailException {
        SailConnection sc;

        if (waitForCommit) {
            sc = new ListenerSailConnection(getBaseSail(), diffSink, true);
        } else {
            // Note: this is a little faster if you don't need to wait for a
            // commit to write statements.
            sc = getBaseSail().getConnection();
            sc.addConnectionListener(new ListenerDiffAdapter(diffSink));
        }

        return sc;
    }
}
