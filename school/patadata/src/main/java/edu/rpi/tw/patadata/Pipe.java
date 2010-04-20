package edu.rpi.tw.patadata;

/**
 * User: josh
 * Date: Apr 16, 2010
 * Time: 5:57:23 PM
 */
public abstract class Pipe<T, S, E extends Exception> implements Handler<T, E> {
    protected final Handler<S, E> innerHandler;

    public Pipe(final Handler<S, E> innerHandler) {
        this.innerHandler = innerHandler;
    }
}
