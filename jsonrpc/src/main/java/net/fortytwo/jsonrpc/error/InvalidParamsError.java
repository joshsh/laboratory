package net.fortytwo.jsonrpc.error;

import net.fortytwo.jsonrpc.JSONRPCError;
import org.json.JSONObject;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:31:18 PM
 */
public class InvalidParamsError extends JSONRPCError {
    public InvalidParamsError(final JSONObject params) {
        super(Type.INVALID_PARAMS.getCode(), "invalid parameters", params);
    }
}