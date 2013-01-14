package net.fortytwo.jsonrpc;

import net.fortytwo.jsonrpc.error.InvalidRequestError;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * An RPC call is represented by sending a Request object to a Server
 * <p/>
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

    /**
     * @param id     an identifier established by the Client that MUST contain a
     *               String, Number, or NULL value if included. If it is not
     *               included it is assumed to be a notification. The value
     *               SHOULD normally not be Null and Numbers SHOULD NOT contain
     *               fractional parts
     * @param method a String containing the name of the method to be invoked.
     *               Method names that begin with the word rpc followed by a
     *               period character (U+002E or ASCII 46) are reserved for
     *               rpc-internal methods and extensions and MUST NOT be used
     *               for anything else.
     * @param params a Structured value that holds the parameter values to be
     *               used during the invocation of the method. This member MAY
     *               be omitted.
     */
    public JSONRPCRequest(final Object id,
                          final String method,
                          final JSONObject params) {
        this.id = id;
        try {
            JSONRPC.validateId(id);
        } catch (InvalidRequestError e) {
            throw new IllegalArgumentException(e.getMessage());
        }

        this.method = method;
        this.params = params;
    }

    /**
     * @param json a valid JSON-RPC request
     * @throws net.fortytwo.jsonrpc.error.InvalidRequestError if the JSON representatio of the request
     *                                 is not valid
     */
    public JSONRPCRequest(final JSONObject json) throws InvalidRequestError {
        id = json.opt(Attrs.ID);
        JSONRPC.validateId(id);

        method = json.optString(Attrs.METHOD);
        // Note: length-0 method names are allowed for now, as they're not explicitly forbidden in the spec.
        if (null == method) {
            throw new InvalidRequestError("missing '" + Attrs.METHOD + "' member");
        }

        params = json.optJSONObject(Attrs.PARAMS);

        String jsonrpc = json.optString(Attrs.JSONRPC);
        if (null == jsonrpc) {
            throw new InvalidRequestError("missing '" + Attrs.JSONRPC
                    + "' member. Use " + Attrs.JSONRPC + ":'" + JSONRPC.VERSION + "'");
        } else if (!jsonrpc.equals(JSONRPC.VERSION)) {
            throw new InvalidRequestError("JSON-RPC version not supported: '"
                    + jsonrpc + "' (should be '" + JSONRPC.VERSION + "')");
        }
    }

    /**
     * @return a String containing the name of the method to be invoked. Method
     *         names that begin with the word rpc followed by a period character
     *         (U+002E or ASCII 46) are reserved for rpc-internal methods and
     *         extensions and MUST NOT be used for anything else.
     */
    public String getMethod() {
        return method;
    }

    /**
     * @return a Structured value that holds the parameter values to be used during the invocation of the method. This member MAY be omitted.
     */
    public JSONObject getParams() {
        return params;
    }

    /**
     * @return an identifier established by the Client that MUST contain a
     *         String, Number, or NULL value if included. If it is not
     *         included it is assumed to be a notification. The value SHOULD
     *         normally not be Null and Numbers SHOULD NOT contain fractional
     *         parts
     */
    public Object getId() {
        return id;
    }

    /**
     * @return whether this is a notification. A Notification is a Request
     *         object without an "id" member. A Request object that is a
     *         Notification signifies the Client's lack of interest in the
     *         corresponding Response object, and as such no Response object
     *         needs to be returned to the client. The Server MUST NOT reply to
     *         a Notification, including those that are within a batch request.
     *         Notifications are not confirmable by definition, since they do
     *         not have a Response object to be returned. As such, the Client
     *         would not be aware of any errors (like e.g. "Invalid params.",
     *         "Internal error.").
     */
    public boolean isNotification() {
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
}
