package net.fortytwo.jsonrpc.error;

import net.fortytwo.jsonrpc.JSONRPCError;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:31:18 PM
 */
public class MethodNotFoundError extends JSONRPCError {
    public MethodNotFoundError(final String name) {
        super(Type.METHOD_NOT_FOUND.getCode(), "method not found", name);
    }
}