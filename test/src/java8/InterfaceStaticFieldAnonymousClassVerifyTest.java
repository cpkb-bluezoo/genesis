/*
 * Regression test: an anonymous class assigned to an INTERFACE's own
 * field, with no explicit "static" keyword (interface fields are always
 * implicitly public static final per JLS 9.3) - matching gumdrop's own
 * ServeStalePolicy, an interface with two constant fields (ENABLED/
 * DISABLED) each initialized by an anonymous implementation of the
 * interface itself. Used to throw at class-initialization time:
 *
 *   java.lang.NoSuchMethodError: Policy$1: method 'void <init>()' not
 *   found
 *
 * Root cause: semantic.c registers an interface field's implicit
 * public/static/final modifiers onto the resolved field SYMBOL only
 * (field_sym->modifiers |= ...) - never onto the raw AST node's own
 * .data.node.flags. A later, separate pass tracks whether an anonymous
 * class's enclosing context is static (deciding whether it needs to
 * capture an enclosing `this`) by reading that AST node's flags
 * directly, not the resolved symbol - so for an interface field with no
 * EXPLICIT "static" keyword in source, that check never saw the
 * implicit static-ness, and the anonymous class implementing the
 * interface got wrongly generated as an inner class capturing a
 * nonexistent enclosing instance (a "this$0" field of the interface's
 * own type, with a constructor requiring that instance as a parameter).
 * There is no actual instance to pass for that capture at the field's
 * own static initialization site, so the anonymous class's real
 * (differently-shaped) constructor was invoked with 0 arguments instead.
 *
 * Fixed by also OR-ing the implicit public/static/final modifiers onto
 * the AST node's own flags, not just the symbol.
 */
public class InterfaceStaticFieldAnonymousClassVerifyTest {
    interface Policy {
        boolean shouldDo();

        Policy ENABLED = new Policy() {
            @Override
            public boolean shouldDo() {
                return true;
            }
        };

        Policy DISABLED = new Policy() {
            @Override
            public boolean shouldDo() {
                return false;
            }
        };
    }

    public static void main(String[] args) {
        if (!Policy.ENABLED.shouldDo()) {
            throw new RuntimeException("expected ENABLED.shouldDo() to be true");
        }
        if (Policy.DISABLED.shouldDo()) {
            throw new RuntimeException("expected DISABLED.shouldDo() to be false");
        }

        System.out.println("InterfaceStaticFieldAnonymousClassVerifyTest passed!");
    }
}
