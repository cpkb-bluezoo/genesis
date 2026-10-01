import boundedctorlib.BoundedBase;

/*
 * Bug: a "super(args)" call to a CLASSFILE-LOADED (compiled separately,
 * referenced via -cp) generic superclass constructor whose parameter is a
 * BOUNDED class-level type variable (e.g. "T extends CharSequence") wrote an
 * invokespecial descriptor using "Ljava/lang/Object;" for that parameter
 * instead of the bound's own descriptor ("Ljava/lang/CharSequence;") -
 * mirrors javax.tools.ForwardingJavaFileManager<M extends JavaFileManager>'s
 * own constructor shape exactly.
 *
 * Root cause: codegen_expr.c's AST_EXPLICIT_CTOR_CALL codegen (for an
 * explicit "super(...)"/"this(...)" call) rebuilds the target constructor's
 * descriptor by calling type_to_descriptor() on each of the resolved target
 * constructor's own declared PARAMETER types - which correctly reads a type
 * variable's bound when one is populated, but a classfile-loaded parameter
 * symbol's type_t doesn't reliably carry that bound at all (populating it
 * would need parsing the declaring class's own generic Signature attribute,
 * which genesis's classfile loader doesn't do for method parameters), so it
 * silently fell back to "unbounded type variable erases to Object" for
 * exactly this shape. Fixed by preferring a classfile-loaded target
 * constructor's own already-erased descriptor (read directly from its
 * classfile, exactly as javac itself already baked the correct erasure into
 * it when compiling the superclass) over reconstructing one parameter type
 * at a time. Confirmed against gumdrop's own
 * InMemoryJavaCompiler$InMemoryFileManager, whose
 * "super(fileManager)" (extending
 * javax.tools.ForwardingJavaFileManager<StandardJavaFileManager>) depends on
 * this exact resolution.
 */
public class BoundedSuperConstructorTest extends BoundedBase<String> {
    public BoundedSuperConstructorTest(String value) {
        super(value);
    }

    public static void main(String[] args) {
        BoundedSuperConstructorTest t = new BoundedSuperConstructorTest("hello");
        if (!"hello".equals(t.value)) {
            throw new RuntimeException("expected value==\"hello\", got " + t.value);
        }
        System.out.println("BoundedSuperConstructorTest passed!");
    }
}
