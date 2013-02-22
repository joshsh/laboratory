/* Copyright (C) 2008 Knowledge Reef Systems.  All rights reserved. */

package com.knowledgereefsystems.agsail.sesameutils.listener;

import com.knowledgereefsystems.reef.onto.KRSException;

import info.aduna.iteration.CloseableIteration;

import org.openrdf.model.Namespace;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;
import org.openrdf.query.Dataset;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.algebra.TupleExpr;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailConnectionListener;
import org.openrdf.sail.SailException;

import net.fortytwo.ripple.flow.diff.DiffBuffer;
import net.fortytwo.ripple.flow.diff.DiffSink;

// TODO: define rollback behavior

// TODO: investigate inference
public class ListenerSailConnection implements SailConnection {
    private SailConnection baseSailConnection;
    private boolean waitForCommit;

    private DiffBuffer<Statement, KRSException> statementDiffBuffer;
    private DiffSink<Statement, KRSException> statementDiffSink;

    public ListenerSailConnection(final Sail baseSail,
                                  final DiffSink<Statement, KRSException> statementDiffSink,
                                  final boolean waitForCommit) throws SailException {
        baseSailConnection = baseSail.getConnection();

        if (waitForCommit) {
            statementDiffBuffer = new DiffBuffer(statementDiffSink);
            this.statementDiffSink = statementDiffBuffer;
        } else {
            this.statementDiffSink = statementDiffSink;
        }

        this.waitForCommit = waitForCommit;

        // A special listener for the statement diff.
        SailConnectionListener listener = new ListenerDiffAdapter(this.statementDiffSink);
        baseSailConnection.addConnectionListener(listener);
    }

    public void addConnectionListener(final SailConnectionListener listener) {
        baseSailConnection.addConnectionListener(listener);
    }

    // Note: adding statements does not change the configuration of cached
    // values.
    public void addStatement(final Resource subj, final URI pred, final Value obj,
                             final Resource... contexts) throws SailException {
        baseSailConnection.addStatement(subj, pred, obj, contexts);
    }

    // Note: clearing statements does not change the configuration of cached
    // values.
    public void clear(final Resource... contexts) throws SailException {
        baseSailConnection.clear(contexts);
    }

    public void clearNamespaces() throws SailException {
        baseSailConnection.clearNamespaces();
    }

    public void close() throws SailException {
        baseSailConnection.close();
    }

    public void commit() throws SailException {
        if (waitForCommit) {
            try {
                statementDiffBuffer.flush();
            } catch (Exception e) {
                throw new SailException(e);
            }
        }
    }

    public CloseableIteration<? extends BindingSet, QueryEvaluationException> evaluate(
            final TupleExpr tupleExpr, final Dataset dataSet, final BindingSet bindingSet, final boolean includeInferred)
            throws SailException {
        return baseSailConnection.evaluate(tupleExpr, dataSet, bindingSet, includeInferred);
    }

    public CloseableIteration<? extends Resource, SailException> getContextIDs()
            throws SailException {
        return baseSailConnection.getContextIDs();
    }

    public String getNamespace(final String prefix) throws SailException {
        return baseSailConnection.getNamespace(prefix);
    }

    public CloseableIteration<? extends Namespace, SailException> getNamespaces()
            throws SailException {
        return baseSailConnection.getNamespaces();
    }

    public CloseableIteration<? extends Statement, SailException> getStatements(
            final Resource subj, final URI pred, final Value obj, final boolean includeInferred, final Resource... contexts)
            throws SailException {
        return baseSailConnection.getStatements(subj, pred, obj, includeInferred, contexts);
    }

    public boolean isOpen() throws SailException {
        return baseSailConnection.isOpen();
    }

    public void removeConnectionListener(final SailConnectionListener listener) {
        baseSailConnection.removeConnectionListener(listener);
    }

    public void removeNamespace(final String prefix) throws SailException {
        baseSailConnection.removeNamespace(prefix);
    }

    // Note: removing statements does not change the configuration of cached
    // values.
    public void removeStatements(final Resource subj, final URI pred, final Value obj,
                                 final Resource... contexts) throws SailException {
        baseSailConnection.removeStatements(subj, pred, obj, contexts);
    }

    // No rollback ability for now.
    public void rollback() throws SailException {
        if (waitForCommit) {
            statementDiffBuffer.clear();
        }

        baseSailConnection.rollback();
    }

    public void setNamespace(final String prefix, final String name) throws SailException {
        baseSailConnection.setNamespace(prefix, name);
    }

    public long size(final Resource... contexts) throws SailException {
        return baseSailConnection.size(contexts);
    }
}