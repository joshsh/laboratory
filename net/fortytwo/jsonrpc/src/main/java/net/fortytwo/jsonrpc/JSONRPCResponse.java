package net.fortytwo.jsonrpc;

import net.fortytwo.jsonrpc.error.InvalidRequestError;
import net.fortytwo.jsonrpc.error.ServerError;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * When an RPC call is made, the Server MUST reply with a Response, except for
 * in the case of Notifications. The Response is expressed as a single JSON
 * Object.
 * <p/>
 * User: josh
 * Date: Nov 4, 2010
 * Time: 6:59:28 PM
 */
public class JSONRPCResponse {
    private interface Attrs {
        public static final String
                JSONRPC = "jsonrpc",
                RESULT = "result",
                ERROR = "error",
                ID = "id";
    }

    private final Object id;
    private final Object result;
    private final JSONRPCError error;

    /**
     * @param id     identifier of the response. This member is REQUIRED. It
     *               MUST be the same as the value of the id member in the
     *               Request Object. If there was an error in detecting the id
     *               in the Request object (e.g. Parse error/Invalid Request),
     *               it MUST be Null.
     * @param result The result of executing the corresponding request. This
     *               member is REQUIRED on success. This member MUST NOT exist
     *               if there was an error invoking the method. The value of
     *               this member is determined  by the method invoked on the
     *               Server.
     * @throws net.fortytwo.jsonrpc.error.ServerError
     *          if the result is invalid
     */
    public JSONRPCResponse(final Object id,
                           final Object result) throws ServerError {
        this.id = id;
        if (null == id) {
            throw new IllegalArgumentException("id of a normal response may not be null");
        }
        try {
            JSONRPC.validateId(id);
        } catch (InvalidRequestError e) {
            throw new IllegalArgumentException(e.getMessage());
        }

        this.result = result;
        JSONRPC.isValidObject(result);

        this.error = null;
    }

    /**
     * @param id    identifier of the response. This member is REQUIRED. It MUST
     *              be the same as the value of the id member in the Request
     *              Object. If there was an error in detecting the id in the
     *              Request object (e.g. Parse error/Invalid Request), it MUST
     *              be Null.
     * @param error an error response. This member is REQUIRED on error. This
     *              member MUST NOT exist if there was no error triggered during
     *              invocation. The value for this member MUST be an Object.
     */
    public JSONRPCResponse(final Object id,
                           final JSONRPCError error) {
        this.id = id;
        try {
            JSONRPC.validateId(id);
        } catch (InvalidRequestError e) {
            throw new IllegalArgumentException(e.getMessage());
        }
        this.result = null;
        this.error = error;
        if (null == error) {
            throw new IllegalArgumentException("null error object");
        }
    }

    public JSONRPCResponse(final JSONObject json) throws InvalidResponseException {
        id = json.opt(Attrs.ID);
        if (null == id) {
            throw new InvalidResponseException("missing '" + Attrs.ID + "' member");
        }
        try {
            JSONRPC.validateId(id);
        } catch (InvalidRequestError e) {
            throw new InvalidResponseException(e.getMessage());
        }

        result = json.opt(Attrs.RESULT);
        JSONObject errObj = json.optJSONObject(Attrs.ERROR);
        error = null == errObj ? null : new JSONRPCError(errObj);

        if (null != result && null != error) {
            throw new InvalidResponseException("both '" + Attrs.RESULT + "' and '" + Attrs.ERROR + "' are non-null");
        }
    }

    /**
     * @return identifier of the response. This member is REQUIRED. It MUST be
     *         the same as the value of the id member in the Request Object. If
     *         there was an error in detecting the id in the Request object
     *         (e.g. Parse error/Invalid Request), it MUST be Null.
     */
    public Object getId() {
        return id;
    }

    /**
     * @return The result of executing the corresponding request. This member is
     *         REQUIRED on success. This member MUST NOT exist if there was an
     *         error invoking the method. The value of this member is determined
     *         by the method invoked on the Server.
     */
    public Object getResult() {
        return result;
    }

    /**
     * @return an error response. This member is REQUIRED on error. This member
     *         MUST NOT exist if there was no error triggered during invocation.
     *         The value for this member MUST be an Object.
     */
    public JSONRPCError getError() {
        return error;
    }

    /**
     * @return a JSON-RPC representation of this response
     * @throws org.json.JSONException this shouldn't happen
     */
    public JSONObject toJSON() throws JSONException {
        JSONObject json = new JSONObject();

        json.put(Attrs.JSONRPC, JSONRPC.VERSION);

        if (null != id) {
            json.put(Attrs.ID, id);
        }

        if (null != result) {
            json.put(Attrs.RESULT, result);
        } else {
            json.put(Attrs.ERROR, error.toJSON());
        }

        return json;
    }
}
