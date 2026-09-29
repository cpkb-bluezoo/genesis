/**
 * Bug: an explicit cast to a class-level or method-level type variable
 * (e.g. "return (A) someExpr;" inside "<A extends BasicFileAttributes>
 * A readAttributes(...) {...}") emitted a checkcast against the type
 * PARAMETER'S OWN NAME ("A") instead of erasing to its bound - "A" is
 * not a real class, so the JVM's own class-loading for that constant
 * pool entry failed at runtime with NoClassDefFoundError /
 * ClassNotFoundException the moment the method actually ran (this is
 * not a VerifyError: the bytecode is well-formed and passes
 * verification, since CONSTANT_Class entries aren't resolved until
 * first use).
 *
 * codegen_expr.c's AST_CAST_EXPR handling determined the checkcast
 * target from type_node->sem_type only when it was TYPE_CLASS; for a
 * cast to a type variable (sem_type->kind == TYPE_TYPEVAR), it fell
 * through to a fallback that used the type node's raw AST name
 * (literally "A") instead of erasing to the type variable's bound (or
 * java.lang.Object if unbounded) - every OTHER type-variable erasure
 * site in this codebase (type_to_descriptor()'s own TYPE_TYPEVAR case,
 * used for method/field descriptors) already does this correctly; only
 * this one cast-expression code path didn't. Confirmed against
 * gumdrop's own MemoryFileSystemProvider.readAttributes() (test
 * support code): "<A extends BasicFileAttributes> A readAttributes(...)
 * { ...; return (A) new MemoryFileAttributes(...); }".
 */
public class CastToBoundedTypeVarVerifyTest {
    static <A extends CharSequence> A identity(String s) {
        return (A) s;
    }

    public static void main(String[] args) {
        String result = identity("hello");
        if (!"hello".equals(result)) {
            throw new RuntimeException("expected 'hello', got '" + result + "'");
        }
    }
}
