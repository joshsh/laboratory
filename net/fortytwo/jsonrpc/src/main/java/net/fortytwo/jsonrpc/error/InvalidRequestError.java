package net.fortytwo.jsonrpc.error;

import net.fortytwo.jsonrpc.JSONRPCError;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:13:48 PM
 */
public class InvalidRequestError extends JSONRPCError {
    public InvalidRequestError(final String message) {
        super(Type.INVALID_REQUEST.getCode(), "invalid request", message);
    }
}
