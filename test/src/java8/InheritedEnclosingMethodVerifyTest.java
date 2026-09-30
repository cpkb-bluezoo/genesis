/*
 * An anonymous class defined inside an instance method of class B extends
 * A, calling A's own INHERITED method unqualified (never overridden by
 * B) - the method's declaring symbol is A, but A is never itself
 * lexically enclosing anything here; B (which extends A) is.
 * codegen_expr.c's unqualified-instance-method-call resolution
 * (AST_METHOD_CALL) detected this wasn't a plain "inherited by our own
 * class" call and asked codegen_load_enclosing_this() to walk the
 * this$0 chain looking for A itself - which walks right past B (the
 * actual, correct enclosing instance to invoke on) all the way to the
 * OUTERMOST enclosing class instead, since A is never literally in that
 * chain: VerifyError "Bad type on operand stack ... Outermost ... is
 * not assignable to A".
 *
 * Confirmed against gumdrop's own CertificateCompressor.BrotliStream
 * (extends Decompressor), whose constructor's anonymous
 * BrotliDefaultHandler calls "emit(data)" unqualified -
 * Decompressor.emit(), inherited (not overridden) by BrotliStream.
 */
public class InheritedEnclosingMethodVerifyTest {
    abstract static class Base {
        void emit(String s) {
            System.out.println("emitted: " + s);
        }
    }

    interface Handler {
        void content(String s);
    }

    static final class Derived extends Base {
        Handler handler;

        Derived() {
            handler = new Handler() {
                @Override
                public void content(String s) {
                    emit(s);
                }
            };
        }
    }

    public static void main(String[] args) {
        Derived d = new Derived();
        d.handler.content("hello");
        System.out.println("InheritedEnclosingMethodVerifyTest passed!");
    }
}
