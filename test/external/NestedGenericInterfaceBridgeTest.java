/*
 * Regression test: a generic interface method (e.g. "void done(T result)"
 * inside "interface Gc<T>") declared in one compilation and implemented by
 * an anonymous class in a *separate, later* compilation (loaded via -cp,
 * not -sourcepath - i.e. from the already-compiled .class file, exactly
 * as happens whenever a multi-module build forks the compiler once per
 * module rather than compiling everything in a single invocation) used to
 * throw at the first call despite compiling and verifying cleanly:
 *
 *   java.lang.AbstractMethodError: Receiver class X does not define or
 *   inherit an implementation of the resolved method 'abstract void
 *   done(java.lang.Object)' of interface Gc
 *
 * codegen.c's codegen_interface_method() (used for an abstract interface
 * method with no body) and codegen_abstract_method() (its counterpart for
 * an abstract method in an abstract class) both built the method's
 * plain/erased descriptor but never populated mi->signature - unlike
 * codegen_method() (used for a method *with* a body), which already calls
 * generate_method_signature() for exactly this purpose. So an interface
 * method whose parameter or return type is a type variable (e.g. "T
 * result") compiled with no Signature attribute at all - only the class's
 * own <T:...> signature survived, not each individual method's. Within a
 * single compilation, this went unnoticed because genesis's own in-memory
 * symbol for the interface method (resolved straight from source) already
 * knew its parameter type was a type variable; the gap only showed up
 * when a *different, later* compilation had to reconstruct that same
 * information by reading the interface back from its compiled .class file
 * - where, without the per-method Signature attribute, only the erased
 * descriptor "(Ljava/lang/Object;)V" survives, with no way to tell that
 * parameter was ever generic.
 *
 * generate_interface_bridges() relies on exactly that information (a
 * parameter or return type resolved to TYPE_TYPEVAR) to decide whether an
 * implementing class needs a synthetic bridge method - so for a classfile-
 * loaded interface, this check silently returned false, no bridge method
 * was generated, and the concrete override alone doesn't satisfy the
 * interface's own erased abstract method - triggering AbstractMethodError
 * the first time the interface reference (rather than the concrete type)
 * is used to make the call.
 *
 * Fixed by having both codegen_interface_method() and
 * codegen_abstract_method() also call generate_method_signature(),
 * mirroring codegen_method()'s existing call.
 */
import lib.NestedCallback;

public class NestedGenericInterfaceBridgeTest {
    static void invoke(NestedCallback.Gc<Integer> cb, int value) {
        /* Calling through the interface type is what actually exercises
         * the erased abstract method - a direct call on the concrete
         * anonymous class type wouldn't need the bridge at all. */
        cb.done(value);
    }

    public static void main(String[] args) {
        final int[] captured = { -1 };
        NestedCallback.Gc<Integer> cb = new NestedCallback.Gc<Integer>() {
            public void done(Integer result) {
                captured[0] = result;
            }
        };
        invoke(cb, 42);
        if (captured[0] != 42) {
            System.out.println("FAILED: expected 42, got " + captured[0]);
            System.exit(1);
        }
        System.out.println("NestedGenericInterfaceBridgeTest passed!");
    }
}
