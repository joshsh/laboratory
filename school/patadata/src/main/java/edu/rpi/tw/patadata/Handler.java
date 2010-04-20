package edu.rpi.tw.patadata;

/**
 * User: josh
 * Date: Apr 16, 2010
 * Time: 4:53:37 PM
 */
public interface Handler<T, E extends Exception> {
    boolean handle(T t) throws E;
}

