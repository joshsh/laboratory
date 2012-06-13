package net.fortytwo.jsonrpc.error;

import net.fortytwo.jsonrpc.JSONRPCError;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:31:18 PM
 */
public class InvalidParamsError extends JSONRPCError {
    public InvalidParamsError(final String message) {
        super(Type.INVALID_PARAMS.getCode(), "invalid parameters", message);
    }
}