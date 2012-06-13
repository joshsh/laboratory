package net.fortytwo.jsonrpc;

import net.fortytwo.jsonrpc.error.InvalidRequestError;
import net.fortytwo.jsonrpc.error.ParseError;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.LinkedList;
import java.util.List;

/**
 * To send several Request objects at the same time, the Client MAY send an
 * Array filled with Request objects.
 * <p/>
 * User: josh
 * Date: Nov 4, 2010
 * Time: 6:59:55 PM
 */
public class JSONRPCBatchRequest {
    // TODO: what about batches with two instances of the same request id?

    private final List<JSONRPCRequest> requests;

    public JSONRPCBatchRequest(final List<JSONRPCRequest> requests) {
        this.requests = requests;
    }

    public JSONRPCBatchRequest(final JSONArray json) throws InvalidRequestError, ParseError {
        requests = new LinkedList<JSONRPCRequest>();
        int l = json.length();
        for (int i = 0; i < l; i++) {
            Object o;
            try {
                o = json.get(i);
            } catch (JSONException e) {
                throw new ParseError(e.getMessage());
            }
            if (!(o instanceof JSONObject)) {
                throw new InvalidRequestError("invalid request in batch");
            }
            requests.add(new JSONRPCRequest((JSONObject) o));
        }
    }

    public List<JSONRPCRequest> getRequests() {
        return requests;
    }

    public JSONArray toJSON() throws JSONException {
        JSONArray json = new JSONArray();

        int i = 0;
        for (JSONRPCRequest r : requests) {
            json.put(i, r.toJSON());
            i++;
        }

        return json;
    }
}
