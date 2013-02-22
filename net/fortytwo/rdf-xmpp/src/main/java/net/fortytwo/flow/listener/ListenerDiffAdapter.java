/* Copyright (C) 2008 Knowledge Reef Systems.  All rights reserved. */

package com.knowledgereefsystems.agsail.sesameutils.listener;

import org.openrdf.sail.SailConnectionListener;
import org.openrdf.model.Statement;

import net.fortytwo.ripple.flow.diff.DiffSink;
import com.knowledgereefsystems.reef.onto.KRSException;

/**
 * Author: josh
 * Date: Mar 7, 2008
 * Time: 2:46:58 PM
 */
class ListenerDiffAdapter implements SailConnectionListener {
    private DiffSink<Statement, KRSException> diffSink;

    public ListenerDiffAdapter(final DiffSink<Statement, KRSException> diffSink) {
        this.diffSink = diffSink;
    }

    public synchronized void statementAdded(final Statement st) {
        try {
            diffSink.getPlus().put(st);
        } catch (KRSException e) {
            e.logError();
        }
    }

    public synchronized void statementRemoved(final Statement st) {
        try {
            diffSink.getMinus().put(st);
        } catch (KRSException e) {
            e.logError();
        }
    }
}
