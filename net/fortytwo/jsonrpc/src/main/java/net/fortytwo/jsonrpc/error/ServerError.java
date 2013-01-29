package net.fortytwo.jsonrpc.error;

import net.fortytwo.jsonrpc.JSONRPCError;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:31:18 PM
 */
public class ServerError extends JSONRPCError {
    public ServerError(final int code,
                       final String message,
                       final Object data) {
        super(code, message, data);
        if (code < SERVER_ERROR_MIN || code > SERVER_ERROR_MAX) {
            throw new IllegalArgumentException("code " + code + " is out of range for a server error"
             + " (it should be between " + SERVER_ERROR_MIN + " and " + SERVER_ERROR_MAX + ")");
        }
    }
}