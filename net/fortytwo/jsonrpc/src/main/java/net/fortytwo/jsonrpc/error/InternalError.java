package net.fortytwo.jsonrpc.error;

import net.fortytwo.jsonrpc.JSONRPCError;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:31:18 PM
 */
public class InternalError extends JSONRPCError {
    public InternalError(final Throwable t) {
        super(Type.INTERNAL_ERROR.getCode(), "internal error", t.getMessage());
    }
}