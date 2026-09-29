package crossfilestaticfieldassign;

/**
 * Assigns to Holder's static fields from a DIFFERENT file than Holder
 * itself - the shape that hits codegen_assignment()'s
 * "receiver is a class name" check in codegen_expr.c, whose only
 * resolver (resolve_class_name()) doesn't know about any class besides
 * a handful of well-known JDK ones, the current class, and a nested
 * class of the current class.
 */
public class User {
    static void setValue(String v) {
        Holder.value = v;
    }

    static void bumpCounter() {
        Holder.counter = 5L;
    }
}
