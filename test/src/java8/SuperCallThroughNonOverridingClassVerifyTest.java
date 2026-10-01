/**
 * Bug: "super.method()", called from a class whose OWN override has a
 * covariant return type, resolved to the WRONG method symbol whenever the
 * immediate superclass itself does NOT override the method (i.e. the real
 * declaration is one or more levels further up the hierarchy). Root cause:
 * semantic.c's method-call type-checking (the AST_METHOD_CALL case of
 * get_expression_type()) special-cases AST_THIS_EXPR ("this.method()") to
 * search starting from the current class, but had NO case at all for
 * AST_SUPER_EXPR ("super.method()") - falling through to a default/fallback
 * resolution that also searched the CURRENT class's own member scope first,
 * finding the current class's OWN covariant override instead of the actual
 * inherited declaration. codegen_expr.c's own super-call handling (which
 * DOES correctly search starting from the immediate superclass) only runs
 * when semantic analysis didn't already resolve a symbol - so the wrong,
 * semantically-resolved symbol from the current class silently won, and the
 * classfile ended up with an invokespecial referencing a method that simply
 * doesn't exist on the immediate superclass (a real, existing method with a
 * DIFFERENT descriptor was fabricated instead): NoSuchMethodError at
 * runtime, despite genesis itself compiling without complaint. Confirmed
 * against gumdrop's own Http2Listener/MqttListener, whose
 * "super.bindWildcard()" (each overriding Listener.bindWildcard() with its
 * own covariant return type, through the non-overriding intermediate
 * TcpListener) depends on this exact resolution.
 */
public class SuperCallThroughNonOverridingClassVerifyTest {
    static class A {
        A self() {
            return this;
        }
    }

    /* Deliberately does NOT override self() - the real declaration a
     * "super.self()" from C needs to reach is one level further up, in A. */
    static class B extends A {
    }

    static class C extends B {
        @Override
        C self() {
            super.self();
            return this;
        }
    }

    public static void main(String[] args) {
        C c = new C();
        if (c.self() != c) {
            throw new RuntimeException("expected self() to return the same instance");
        }
    }
}
