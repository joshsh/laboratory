package net.fortytwo.myotherbrain.web.jsonrpc;

import net.fortytwo.myotherbrain.web.jsonrpc.error.InvalidRequestException;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 6:59:01 PM
 */
public class JSONRPCRequest {
    private interface Attrs {
        public static final String
                JSONRPC = "jsonrpc",
                METHOD = "method",
                PARAMS = "params",
                ID = "id";
    }

    private final String method;
    private final JSONObject params;
    private final Object id;

    public JSONRPCRequest(final Object id,
                          final String method,
                          final JSONObject params) throws InvalidRequestException {
        this.id = id;
        validateId(id);

        this.method = method;
        this.params = params;
    }

    public JSONRPCRequest(final JSONObject json) throws InvalidRequestException {
        method = json.optString(Attrs.METHOD);
        // Note: length-0 method names are allowed for now, as they're not explicitly forbidden in the spec.
        if (null == method) {
            throw new InvalidRequestException("missing '" + Attrs.METHOD + "' member");
        }
        params = json.optJSONObject(Attrs.PARAMS);
        id = json.opt(Attrs.ID);
        validateId(id);
        String jsonrpc = json.optString(Attrs.JSONRPC);
        if (null == jsonrpc) {
            throw new InvalidRequestException("missing '" + Attrs.JSONRPC
                    + "' member. Use " + Attrs.JSONRPC + ":'" + JSONRPC.VERSION + "'");
        } else if (!jsonrpc.equals(JSONRPC.VERSION)) {
            throw new InvalidRequestException("JSON-RPC version not supported: '"
                    + jsonrpc + "' (should be '" + JSONRPC.VERSION + "')");
        }
    }

    public String getMethod() {
        return method;
    }

    public JSONObject getParams() {
        return params;
    }

    public Object getId() {
        return id;
    }

    public boolean isNotificationRequest() {
        return null == id;
    }

    /**
     * @return a JSON-RPC representation of this request
     * @throws JSONException this shouldn't happen
     */
    public JSONObject toJSON() throws JSONException {
        JSONObject json = new JSONObject();
        json.put(Attrs.JSONRPC, JSONRPC.VERSION);
        if (null != id) {
            json.put(Attrs.ID, id);
        }
        json.put(Attrs.METHOD, method);
        json.put(Attrs.PARAMS, params);
        return json;
    }

    private static void validateId(final Object id) throws InvalidRequestException {
        if (null != id && !(id instanceof String) && !(id instanceof Number)) {
            throw new InvalidRequestException("non-null id is neither a string nor a number: '" + id + "'");
        }
    }
}
