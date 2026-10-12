/**
 * Regression: bound instance method ref to a generic SAM must specialize the
 * metafactory return to the implementation return type, not Object (Spring
 * graphQlRouterFunction: httpHandler::handleRequest as HandlerFunction).
 */
public class MethodRefBoundSamReturnVerifyTest {

    interface ServerRequest {
    }

    interface ServerResponse {
    }

    interface HandlerFunction<T extends ServerResponse> {
        T handle(ServerRequest request);
    }

    static final class HttpHandler {
        ServerResponse handleRequest(ServerRequest request) {
            return new ServerResponse() {
            };
        }
    }

    static void route(HandlerFunction<ServerResponse> handler) {
        if (handler.handle(null) == null) {
            throw new RuntimeException("null response");
        }
    }

    public static void main(String[] args) {
        HttpHandler httpHandler = new HttpHandler();
        route(httpHandler::handleRequest);
        System.out.println("PASS");
    }
}
