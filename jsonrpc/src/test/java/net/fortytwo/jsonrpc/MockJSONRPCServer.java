package net.fortytwo.jsonrpc;

import net.fortytwo.jsonrpc.error.InvalidParamsError;
import net.fortytwo.jsonrpc.error.ServerError;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Map;

/**
 * User: josh
 * Date: Nov 14, 2010
 * Time: 8:35:12 PM
 */
public class MockJSONRPCServer extends JSONRPCServer {
    public MockJSONRPCServer() {
        this.registerMethod(new ExampleMethodWithNamedParams());
        this.registerMethod(new ExampleMethodWithOrderedParams());
        this.registerMethod(new ExampleMethodWithStructuredResult());
        this.registerMethod(new ExampleMethodWithNoParams());
        this.registerMethod(new ExampleNotificationMethod());
    }

    private class ExampleMethodWithNamedParams extends JSONRPCMethod {

        public String getName() {
            return "exampleMethodWithNamedParams";
        }

        @Override
        protected Object execute(final Map<String, Object> namedParams) throws InvalidParamsError, ServerError {
            Object first = namedParams.get("first");
            if (null == first || !(first instanceof String)) {
                throw new InvalidParamsError("missing or invalid 'first' parameter");
            }

            Object second = namedParams.get("second");
            if (null == second || !(second instanceof String)) {
                throw new InvalidParamsError("missing or invalid 'second' parameter");
            }

            return ((String) first).concat((String) second);
        }
    }

    private class ExampleMethodWithOrderedParams extends JSONRPCMethod {

        public String getName() {
            return "exampleMethodWithOrderedParams";
        }

        @Override
        protected Object execute(final Object... orderedParams) throws InvalidParamsError, ServerError {
            if (2 != orderedParams.length) {
                throw new InvalidParamsError("wrong number of parameters");
            }

            Object first = orderedParams[0];
            Object second = orderedParams[1];

            return ((String) first).concat((String) second);
        }
    }

    private class ExampleMethodWithStructuredResult extends JSONRPCMethod {

        public String getName() {
            return "exampleMethodWithStructuredResult";
        }

        @Override
        protected Object execute(final Object... orderedParams) throws InvalidParamsError, ServerError {
            if (2 != orderedParams.length) {
                throw new InvalidParamsError("wrong number of parameters");
            }

            Object first = orderedParams[0];
            Object second = orderedParams[1];

            JSONObject result = new JSONObject();
            try {
                result.put("first", first);
                result.put("second", second);
            } catch (JSONException e) {
                throw new ServerError(JSONRPCError.SERVER_ERROR_MIN + 1, "json error", null);
            }

            return result;
        }
    }

    private class ExampleMethodWithNoParams extends JSONRPCMethod {

        public String getName() {
            return "exampleMethodWithNoParams";
        }

        @Override
        protected Object execute(final Object... orderedParams) throws InvalidParamsError, ServerError {
            return 42;
        }
    }

    private class ExampleNotificationMethod extends JSONRPCMethod {

        public String getName() {
            return "exampleNotificationMethod";
        }

        @Override
        protected Object execute(final Object... orderedParams) throws InvalidParamsError, ServerError {
            if (2 != orderedParams.length) {
                throw new InvalidParamsError("wrong number of parameters");
            }

            return null;
        }
    }
}
