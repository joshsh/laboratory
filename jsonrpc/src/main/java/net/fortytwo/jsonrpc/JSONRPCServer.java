package net.fortytwo.jsonrpc;

import net.fortytwo.jsonrpc.error.InternalError;
import net.fortytwo.jsonrpc.error.MethodNotFoundError;
import net.fortytwo.jsonrpc.error.ParseError;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:48:37 PM
 */
public class JSONRPCServer {
    private final Map<String, JSONRPCMethod> methodsByName;

    public JSONRPCServer() {
        methodsByName = new HashMap<String, JSONRPCMethod>();
    }

    /**
     * @param method a new method for executing requests
     */
    public synchronized void registerMethod(final JSONRPCMethod method) {
        if (null == method) {
            throw new IllegalArgumentException("null method");
        }

        String name = method.getName();
        if (null == name) {
            throw new IllegalArgumentException("null method name");
        }

        if (null != methodsByName.get(method.getName())) {
            throw new IllegalArgumentException("a method named '" + name + "' has already been registered");
        }

        methodsByName.put(name, method);
    }

    public String handle(final String request) {
        try {
            return handleInternal(request.trim());
        } catch (JSONRPCError e) {
            try {
                return e.toJSON().toString();
            } catch (JSONException e1) {
                throw new IllegalStateException(e1);
            }
        }
    }

    /**
     * Performs a procedure call.
     *
     * @param request RPC request
     * @return the RPC response. If the request is a notification, this will be null.
     * @throws JSONRPCError
     */
    public JSONRPCResponse handle(final JSONRPCRequest request) throws JSONRPCError {
        JSONRPCMethod m = methodsByName.get(request.getMethod());
        if (null == m) {
            throw new MethodNotFoundError(request.getMethod());
        }

        JSONObject params = request.getParams();
        // For now, default to ordered params when none are specified
        Object result = m.execute(null == params ? new JSONObject() : request.getParams());
        JSONRPCResponse response = new JSONRPCResponse(request.getId(), result);
        return request.isNotification() ? null : response;
    }

    // TODO: give the application developer control over the order of execution of requests
    public JSONRPCBatchResponse handle(final JSONRPCBatchRequest request) throws JSONRPCError {
        List<JSONRPCResponse> responses = new LinkedList<JSONRPCResponse>();

        for (JSONRPCRequest r : request.getRequests()) {
            JSONRPCResponse response = handle(r);
            if (!r.isNotification()) {
                responses.add(response);
            }
        }

        return new JSONRPCBatchResponse(responses);
    }

    ////////////////////////////////////////////////////////////////////////////

    private String handleInternal(final String request) throws JSONRPCError {
        String r = request.trim();
        if (r.startsWith("[")) {
            JSONArray json;
            try {
                json = new JSONArray(r);
            } catch (JSONException e) {
                throw new ParseError(e.getMessage());
            }
            try {
                JSONRPCBatchResponse response = handle(new JSONRPCBatchRequest(json));

                // "If there are no Response objects contained within the
                // Response array as it is to be sent to the client, the
                // server MUST NOT return an empty Array and should return
                // nothing at all."
                if (0 == response.getResponses().size()) {
                    return "";
                } else {
                    return response.toJSON().toString();
                }
            } catch (JSONException e) {
                throw new InternalError(e);
            }
        } else if (r.startsWith("{")) {
            JSONObject json;
            try {
                json = new JSONObject(r);
            } catch (JSONException e) {
                throw new ParseError(e.getMessage());
            }
            try {
                JSONRPCRequest req = new JSONRPCRequest(json);
                JSONRPCResponse response = handle(req);
                return req.isNotification() ? "" : response.toJSON().toString();
            } catch (JSONException e) {
                throw new InternalError(e);
            }
        } else {
            throw new ParseError("request is neither a valid JSON object nor JSON array");
        }
    }
}
