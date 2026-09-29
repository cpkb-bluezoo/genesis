import java.util.HashMap;
import java.util.Map;

/*
 * Regression test: passing `this` as an argument to a method whose
 * parameter type is a reference type (e.g. "Map<String, X>.put(String,
 * X)") - matching gumdrop's own test-only DnsClientTransport
 * implementations, which do "harness.transports.put(server.
 * getHostAddress(), this);" inside their own open() override. Used to
 * throw at class-verification time:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type '...' (current frame, stack[N]) is not assignable to
 *   integer
 *
 * with an unexpected "invokestatic java/lang/Integer.valueOf:(I)
 * Ljava/lang/Integer;" inserted right before the invokeinterface for
 * put(), as if "this" needed boxing from a primitive int.
 *
 * Root cause: codegen_expr.c's get_expr_type_kind() - used by argument-
 * boxing decisions to determine an argument expression's own type kind -
 * had no case for AST_THIS_EXPR at all, and AST_THIS_EXPR nodes carry no
 * sem_type either (already noted elsewhere in the same file), so "this"
 * fell through every branch straight to the function's own final
 * "default to TYPE_INT" fallback (the same fallback used for a
 * genuinely unresolvable expression). A caller deciding whether "this"
 * needed boxing before being passed as a reference-typed argument then
 * wrongly treated it as a primitive int, inserting a bogus
 * Integer.valueOf(I) call before the real invocation.
 *
 * Fixed by adding an explicit TYPE_CLASS case for AST_THIS_EXPR.
 */
public class ThisArgumentWrongBoxingVerifyTest {
    interface Thing {
        void register(String key);
    }

    static class Registry {
        final Map<String, ManualThing> things = new HashMap<String, ManualThing>();
    }

    static class ManualThing implements Thing {
        final Registry registry;

        ManualThing(Registry registry) {
            this.registry = registry;
        }

        @Override
        public void register(String key) {
            registry.things.put(key, this);
        }
    }

    public static void main(String[] args) {
        Registry registry = new Registry();
        ManualThing thing = new ManualThing(registry);
        thing.register("hello");

        if (registry.things.get("hello") != thing) {
            throw new RuntimeException("expected registry to hold the registered instance");
        }

        System.out.println("ThisArgumentWrongBoxingVerifyTest passed!");
    }
}
