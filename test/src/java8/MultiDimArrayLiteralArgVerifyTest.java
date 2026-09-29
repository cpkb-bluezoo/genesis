/*
 * Regression test: a multi-dimensional array-initializer literal (`new
 * int[][] { { 2, 1 } }`) passed directly as an argument to a plain
 * (non-varargs) method expecting that array type - exactly gumdrop's
 * own TcpTransportFactoryEchClientTest shape:
 *
 *   config(9, "unusable.example", new int[][] { { 2, 1 } })
 *   // where config(int id, String publicName, int[][] suites) { ... }
 *
 * used to throw:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Type '[Ljava/lang/Object;' ... is not assignable to '[[I'
 *
 * Root cause: the parser's TOK_NEW handler for `new int[][] {...}`
 * receives an already-doubly-nested AST_ARRAY_TYPE(AST_ARRAY_TYPE(int))
 * element-type node (parse_type() greedily consumes both trailing empty
 * bracket pairs itself before the TOK_NEW handler sees them), then only
 * unwraps ONE level - so the AST_NEW_ARRAY node's element-type child
 * ends up as "int[]" (an AST_ARRAY_TYPE), not the primitive "int",
 * losing the "this is 2 dimensions" information at the parser level.
 * semantic.c's own AST_NEW_ARRAY handling in get_expression_type()
 * already works around this (flattening nested AST_ARRAY_TYPE element
 * types and computing the correct dimensions/element_type onto
 * array_init->sem_type) - but codegen_new_array()'s own hand-rolled
 * initializer-handling branch (codegen_expr.c) never consulted that
 * already-correct sem_type at all: it derived everything from the same
 * (dimension-losing) type_node instead, so a multi-dimensional literal
 * fell into its reference-type/ANEWARRAY branch with no real class name
 * available, silently defaulting to "java/lang/Object" - producing an
 * Object[] instead of int[][], rejected the moment that value was used
 * somewhere requiring the real array type.
 *
 * Fixed by deleting that ~200-line hand-rolled branch entirely and
 * delegating to the already-correct, already-recursive
 * codegen_array_init() (which does read array_init->sem_type and
 * handles any dimension depth) - the same function already used when an
 * array initializer appears directly as an expression (e.g. a field
 * initializer), just never reached from within codegen_new_array()'s
 * own "new Type[]{...}" handling before this fix.
 */
public class MultiDimArrayLiteralArgVerifyTest {
    static int sum2d(int[][] grid) {
        int total = 0;
        for (int[] row : grid) {
            for (int v : row) {
                total += v;
            }
        }
        return total;
    }

    static String describe(int id, String name, int[][] suites) {
        return id + ":" + name + ":" + suites.length + "x" + suites[0].length;
    }

    public static void main(String[] args) {
        int total = sum2d(new int[][] { { 2, 1 }, { 3, 4 } });
        if (total != 10) {
            throw new AssertionError("expected 10, got " + total);
        }

        String d = describe(9, "unusable.example", new int[][] { { 2, 1 } });
        if (!d.equals("9:unusable.example:1x2")) {
            throw new AssertionError("unexpected: " + d);
        }

        System.out.println("MultiDimArrayLiteralArgVerifyTest passed!");
    }
}
