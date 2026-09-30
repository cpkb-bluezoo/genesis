package wcimport.server;

/*
 * A method parameter typed via a WILDCARD import ONLY (no explicit
 * single-type import, and not the same package as its own declaring
 * class) resolved fine within its own declaring file, but silently
 * dropped to no type at all when read from a DIFFERENT file's own call
 * site in the same compile batch - Phase 2b's own type-name
 * qualification pass (resolve_type_name_with_imports(), semantic.c)
 * checked classpath jars and -sourcepath directory trees for an
 * on-demand ("import pkg.*;") import, but never the in-memory type
 * REGISTRY - where every source file passed directly on genesis's own
 * command line (the common case, no -sourcepath needed) is already
 * registered by the time this pass runs. The parameter's type node
 * stayed an unqualified bare name, Phase 3's own eager parameter-type
 * resolution (which ALSO only ever walks the registry, with the exact
 * same missing wildcard-import check) then failed too, leaving the
 * parameter's own type NULL forever - and
 * build_method_descriptor_from_symbol() (codegen_expr.c) silently
 * defaulted an unresolved parameter type to "I" when building ANY
 * OTHER file's own call site, producing an invokevirtual descriptor
 * that doesn't match the real method at all: VerifyError "Bad type on
 * operand stack ... is not assignable to integer". Confirmed against
 * gumdrop's own MqttServer.createProtocolHandler()'s
 * "handler.setConnectHandler(ch)", where
 * MqttProtocolHandler.setConnectHandler's own "ConnectHandler"
 * parameter is visible only via
 * "import org.bluezoo.gumdrop.mqtt.server.*;".
 */
import wcimport.Handler;

public class WildcardImportParamTypeTest {
    public static void main(String[] args) {
        Handler h = new Handler();
        final boolean[] connected = { false };
        ConnectHandler ch = new ConnectHandler() {
            public void connect() {
                connected[0] = true;
            }
        };
        h.setConnectHandler(ch);
        h.fire();
        if (!connected[0]) {
            System.out.println("FAILED: expected connect() to have run");
            System.exit(1);
        }
        System.out.println("WildcardImportParamTypeTest passed!");
    }
}
