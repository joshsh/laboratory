package net.fortytwo.myotherbrain.web.jsonrpc;

import net.fortytwo.myotherbrain.web.jsonrpc.error.InvalidParamsException;
import net.fortytwo.myotherbrain.web.jsonrpc.error.MethodNotFoundException;
import net.fortytwo.myotherbrain.web.jsonrpc.error.ServerError;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:48:37 PM
 */
public class JSONRPCServer {
    private final Map<String, Method> methodsByName;

    public JSONRPCServer() {
        methodsByName = new HashMap<String, Method>();
    }

    protected abstract Object handleRequest(SimpleRequest request);

    protected abstract List<Object> handleBatchRequest(List<SimpleRequest> requests);

    public String handle(final String requestOrBatch) {

    }

    private JSONRPCResponse handle(final JSONRPCRequest request) throws MethodNotFoundException, ServerError, InvalidParamsException {
        Method m = methodsByName.get(request.getMethod());
        if (null == m) {
            throw new MethodNotFoundException(request.getMethod());
        }

        Object result = m.execute(request.getParams());
        validateResult(result);
    }

    private void validateResult(final Object result) throws ServerError {
        if (!(result instanceof JSONObject)
                && !(result instanceof JSONArray)
                && !(result instanceof String)
                && !(result instanceof Number)
                && !(result instanceof Boolean)) {
            throw new ServerError(JSONRPC.NON_JSON_RESULT, "result object is not compatible with the JSON data model: " + result);
        }
    }

    public synchronized void registerMethod(final Method method) {
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

    public class SimpleRequest {
        private final String method;
        private final JSONObject params;

        public SimpleRequest(final String method,
                             final JSONObject params) {
            this.method = method;
            this.params = params;
        }

        public String getMethod() {
            return method;
        }

        public JSONObject getParams() {
            return params;
        }
    }

}
