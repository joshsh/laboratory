package net.fortytwo.jsonrpc;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * User: josh
 * Date: Nov 14, 2010
 * Time: 1:13:25 PM
 */
public class JSONRPCError extends Exception {
    public enum Type {
        /**
         * JSON-RPC error codes and ranges
         */
        PARSE_ERROR(-32700),
        INVALID_REQUEST(-32600),
        METHOD_NOT_FOUND(-32601),
        INVALID_PARAMS(-32602),
        INTERNAL_ERROR(-32603),

        /**
         * Implementation-specific error codes
         */
        NON_JSON_RESULT(-32001),
        NAMED_PARAMETERS_NOT_SUPPORTED(-32002),
        ORDERED_PARAMETERS_NOT_SUPPORTED(-32003);

        private final int code;

        private Type(final int code) {
            this.code = code;
        }

        public int getCode() {
            return code;
        }
    }

    protected static final int
            SERVER_ERROR_MIN = -32099,
            SERVER_ERROR_MAX = -32000;

    private interface Attrs {
        public static final String
                CODE = "code",
                MESSAGE = "message",
                DATA = "data";
    }

    private final Integer code;
    private final String message;
    private final Object data;

    public JSONRPCError(int code, String message, Object data) {
        this.code = code;
        this.message = message;
        this.data = data;

        if (null != data
                && !(data instanceof Number
                || data instanceof String
                || data instanceof JSONObject
                || data instanceof JSONArray)) {
            throw new IllegalArgumentException("'" + Attrs.DATA + "' member has bad type");
        }
    }

    public JSONRPCError(final JSONObject json) throws InvalidResponseException {
        code = json.optInt(Attrs.CODE);
        if (null == code) {
            throw new InvalidResponseException("missing '" + Attrs.CODE + "' member");
        }

        message = json.optString(Attrs.MESSAGE);
        if (null == message) {
            throw new InvalidResponseException("missing '" + Attrs.MESSAGE + "' member");
        }

        data = json.opt(Attrs.DATA);
    }

    public int getCode() {
        return code;
    }

    public String getMessage() {
        return message;
    }

    public Object getData() {
        return data;
    }

    /**
     * @return a JSON-RPC representation of this error
     * @throws JSONException this shouldn't happen
     */
    public JSONObject toJSON() throws JSONException {
        JSONObject json = new JSONObject();
        json.put(Attrs.CODE, code);

        json.put(Attrs.MESSAGE, message);

        if (null != data) {
            json.put(Attrs.DATA, data);
        }

        return json;
    }
}
