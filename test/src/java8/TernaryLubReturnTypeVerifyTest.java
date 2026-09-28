/*
 * Regression test: a ternary (conditional) expression whose two branches are
 * both non-null reference types, where one is a strict subtype of the
 * other (e.g. `flag ? this : someMethodDeclaringASupertype()`), used to
 * crash the JVM verifier with:
 *
 *   java.lang.VerifyError: Instruction type does not match stack map
 *   Reason: Type '<supertype>' (current frame, stack[k]) is not assignable
 *   to '<subtype>' (stack map, stack[k])
 *
 * get_expression_type()'s AST_CONDITIONAL_EXPR case in semantic.c computed
 * the ternary's overall type (expr->sem_type) as simply "the then branch's
 * type", with a comment acknowledging this was an incomplete stand-in for a
 * real JLS 15.25 least-upper-bound computation. codegen_expr.c's
 * AST_CONDITIONAL_EXPR handling (fixed earlier for TernaryNullMergeVerifyTest)
 * correctly uses this type to reframe the stack map at the ternary's join
 * point - but when the then branch's type is narrower than the else
 * branch's, that frame ends up declaring the narrower type even though the
 * else branch's (wider) value can also reach it, which the verifier
 * rejects. This exact shape occurs in gumdrop's
 * MemoryPath.toAbsolutePath(): "return absolute ? this :
 * fs.rootPath.resolve(this);", where `this` is the concrete MemoryPath and
 * resolve(...) is declared to return the wider Path interface.
 *
 * Fixed by preferring whichever branch's type the other is assignable to
 * (i.e. the supertype), when one is simply a subtype of the other - not a
 * full LUB algorithm for the general case (unrelated types still fall back
 * to the then branch's type, as before), but enough for this concrete
 * "narrower `this` vs. a wider declared return type" case.
 */
public class TernaryLubReturnTypeVerifyTest {
    interface Shape {
        String show();
    }

    static class Circle implements Shape {
        boolean isUnitCircle;
        ShapeFactory factory;

        public String show() {
            return "circle";
        }

        Shape normalize() {
            return isUnitCircle ? this : factory.makeUnitCircle(this);
        }
    }

    static class ShapeFactory {
        Shape makeUnitCircle(Circle c) {
            return new Circle();
        }
    }

    public static void main(String[] args) {
        Circle c = new Circle();
        c.factory = new ShapeFactory();

        c.isUnitCircle = true;
        if (!"circle".equals(c.normalize().show())) {
            throw new RuntimeException("expected normalize() to return the same circle");
        }

        c.isUnitCircle = false;
        if (!"circle".equals(c.normalize().show())) {
            throw new RuntimeException("expected normalize() to return a factory-made circle");
        }

        System.out.println("All tests passed!");
    }
}
