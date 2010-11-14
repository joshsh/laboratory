package net.fortytwo.jsonrpc.error;

import net.fortytwo.jsonrpc.JSONRPCError;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:31:18 PM
 */
public class ParseError extends JSONRPCError {
    public ParseError(final String message) {
        super(Type.PARSE_ERROR.getCode(), "JSON parse error", message);
    }
}
