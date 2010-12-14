package com.tinkerpop.blueprints.sail;

import java.util.Iterator;

/**
 * User: josh
 * Date: Dec 13, 2010
 * Time: 1:45:49 AM
 */
class FilteredIterator<T> implements Iterator<T> {
    private final Iterator<T> baseIterator;
    private final Criterion<T> criterion;
    private T cur;

    public FilteredIterator(final Iterator<T> baseIterator,
                            final Criterion<T> criterion) {
        this.baseIterator = baseIterator;
        this.criterion = criterion;
        advanceToNext();
    }

    public boolean hasNext() {
        return null != cur;
    }

    public T next() {
        return cur;
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }

    private void advanceToNext() {
        while (baseIterator.hasNext()) {
            cur = baseIterator.next();
            if (criterion.passes(cur)) {
                return;
            }
        }

        cur = null;
    }

    public interface Criterion<T> {
        boolean passes(T t);
    }
}