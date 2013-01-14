package net.fortytwo.jsonrpc;

import org.json.JSONArray;
import org.json.JSONException;

import java.util.List;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 7:00:04 PM
 */
public class JSONRPCBatchResponse {
    private final List<JSONRPCResponse> responses;

    public JSONRPCBatchResponse(List<JSONRPCResponse> responses) {
        this.responses = responses;
    }

    public List<JSONRPCResponse> getResponses() {
        return responses;
    }

    public JSONArray toJSON() throws JSONException {
        JSONArray json = new JSONArray();

        int i = 0;
        for (JSONRPCResponse r : responses) {
            json.put(i, r.toJSON());
            i++;
        }

        return json;
    }
}
