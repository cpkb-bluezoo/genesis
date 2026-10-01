package importedannotest.main;

import importedannotest.lib.Outer;
import java.lang.reflect.Method;

/* Regression test: an annotation type nested inside a DIFFERENT, unrelated
 * class (Outer, in another package) and used only by importing that
 * ENCLOSING class - "import importedannotest.lib.Outer; ... @Marker(99)" -
 * compiled in the SAME batch as Outer itself (no classfile for it exists
 * yet). Method.getAnnotation(...) returned null instead of the real
 * annotation instance; no RuntimeVisibleAnnotations entry was written at
 * all.
 *
 * Root cause: semantic_resolve_annotation_retention() (semantic.c) can
 * find a same-batch annotation type's own @Retention meta-annotation two
 * ways: (1) nested under the class currently being compiled (GitHub issue
 * #1's fix - doesn't apply here, Marker is nested in the unrelated Outer,
 * not in this test class or its ancestors), or (2) via the shared type
 * registry, for a type reached through an import rather than nesting.
 * Path (2) checked `reg_sym->kind == SYM_ANNOTATION`, but the registry's
 * own stub for ANY nested type declaration collapses both
 * AST_INTERFACE_DECL and AST_ANNOTATION_DECL to the single kind
 * SYM_INTERFACE (create_type_stub(), genesis.c - there is no
 * SYM_ANNOTATION case there), so that check could never be true and path
 * (2) silently always fell through to CLASS retention. Fixed by checking
 * the registry's own AST for the type instead (reg_ast->type ==
 * AST_ANNOTATION_DECL via type_registry_get_ast()), which correctly
 * distinguishes a real annotation declaration regardless of the stub's
 * collapsed symbol kind.
 *
 * Uses the bare simple name "Marker" (relying on resolve_import()'s
 * "nested type of a single-type-imported class" search, since only Outer
 * itself is imported) rather than the qualified "Outer.Marker" form, to
 * exercise that same resolution path.
 *
 * MUST be compiled into a freshly emptied output directory (see
 * run-tests.sh): genesis puts its own -d directory on the classpath, and
 * class files left there by an earlier compile can mask this bug. */
public class ImportedAnnotationRetentionVerifyTest {
    @Marker(99)
    public void foo() {}

    public static void main(String[] args) throws Exception {
        Method m = ImportedAnnotationRetentionVerifyTest.class.getMethod("foo");
        Outer.Marker marker = m.getAnnotation(Outer.Marker.class);
        if (marker == null) {
            throw new RuntimeException("@Marker missing at runtime - imported same-batch annotation retention not resolved");
        }
        if (marker.value() != 99) {
            throw new RuntimeException("expected 99 but got " + marker.value());
        }
        System.out.println("PASS");
    }
}
