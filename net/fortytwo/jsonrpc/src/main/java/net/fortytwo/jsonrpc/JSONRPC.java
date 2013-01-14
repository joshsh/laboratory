package net.fortytwo.jsonrpc;

import net.fortytwo.jsonrpc.error.InvalidRequestError;
import net.fortytwo.jsonrpc.error.ServerError;
import org.json.JSONObject;
import org.json.JSONArray;

/**
 * JSON-RPC is a stateless, light-weight remote procedure call (RPC) protocol.
 * This implementation deals with JSON-RPC 2.0, as specified in http://groups.google.com/group/json-rpc/web/json-rpc-2-0
 * <p/>
 * User: josh
 * Date: Nov 4, 2010
 * Time: 6:53:50 PM
 */
public class JSONRPC {
    public static final String VERSION = "2.0";

    public static void validateId(final Object id) throws InvalidRequestError {
        if (null != id && !(id instanceof String) && !(id instanceof Number)) {
            throw new InvalidRequestError("non-null id is neither a string nor a number: '" + id + "'");
        }    
    }

    public static boolean isValidObject(final Object result) throws ServerError {
        return (result instanceof JSONObject
                || result instanceof JSONArray
                || result instanceof String
                || result instanceof Number
                || result instanceof Boolean);
    }
}
