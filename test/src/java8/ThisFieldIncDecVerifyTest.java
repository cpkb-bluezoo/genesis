/**
 * Bug: an explicit "this.field++"/"this.field--" (or prefix "++this.field")
 * always used the placeholder java.lang.Object/int fieldref codegen_expr.c's
 * AST_FIELD_ACCESS increment/decrement path falls back to when it can't
 * determine the receiver's real class and the field's real type - which it
 * never could for a "this" receiver (AST_THIS_EXPR), since that node's
 * sem_type is computed on demand by get_expression_type() but never cached
 * onto the node itself, unlike an ordinary object-reference receiver (e.g.
 * "obj.field++"). The written class file references a fieldref on
 * java.lang.Object of type int - NoSuchFieldError at runtime for ANY
 * "this.field" increment/decrement, on any field type, not just wide ones.
 * Confirmed against gumdrop's own Quota.incrementMessageCount(), whose
 * "this.messageCount++" on a long field hits exactly this.
 */
public class ThisFieldIncDecVerifyTest {
    private int narrow;
    private long wide;
    private double fpWide;

    void incNarrowPost() {
        this.narrow++;
    }

    void incNarrowPre() {
        ++this.narrow;
    }

    void incWidePost() {
        this.wide++;
    }

    void decWidePre() {
        --this.wide;
    }

    void incFpWide() {
        this.fpWide++;
    }

    public static void main(String[] args) {
        ThisFieldIncDecVerifyTest t = new ThisFieldIncDecVerifyTest();

        t.incNarrowPost();
        t.incNarrowPre();
        if (t.narrow != 2) {
            throw new RuntimeException("expected narrow==2, got " + t.narrow);
        }

        t.incWidePost();
        t.incWidePost();
        if (t.wide != 2L) {
            throw new RuntimeException("expected wide==2, got " + t.wide);
        }
        t.decWidePre();
        if (t.wide != 1L) {
            throw new RuntimeException("expected wide==1 after decrement, got " + t.wide);
        }

        t.incFpWide();
        if (t.fpWide != 1.0) {
            throw new RuntimeException("expected fpWide==1.0, got " + t.fpWide);
        }
    }
}
