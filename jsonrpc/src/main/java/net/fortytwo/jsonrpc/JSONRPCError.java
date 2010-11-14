package net.fortytwo.myotherbrain.web.jsonrpc;

import net.fortytwo.myotherbrain.web.jsonrpc.error.InvalidResponseException;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * User: josh
 * Date: Nov 14, 2010
 * Time: 1:13:25 PM
 */
public class JSONRPCError {
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
