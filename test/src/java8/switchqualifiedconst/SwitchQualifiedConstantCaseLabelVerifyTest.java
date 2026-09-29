package switchqualifiedconst;

/*
 * Regression test: a switch statement's case label referencing a
 * qualified static-final-int constant from another type declared in the
 * same compilation batch (`case SwitchConstants.START:`), matching the
 * real-world shape of gumdrop's
 * `AmqpClientProtocolHandler.dispatchConnectionMethod`, which switches
 * on `case AmqpMethod.CONNECTION_START:` (AmqpMethod living in a
 * different package, reached only via an explicit import, but compiled
 * together with the switch's own file in the same javac/genesis
 * invocation - the normal way a multi-file project is built). This used
 * to compile silently but generate a broken lookupswitch with every
 * case's match key read as 0:
 *
 *   java.lang.VerifyError: Bad lookupswitch instruction
 *
 * Root cause: semantic.c's switch-statement case-label processing had
 * no branch at all for an AST_FIELD_ACCESS case label (only bare
 * AST_LITERAL and AST_IDENTIFIER were handled), so a qualified
 * constant's case label kept its default int_val of 0 all the way to
 * codegen. A first fix attempt tried resolving the qualified constant
 * via the general get_expression_type() field-access machinery, but
 * that function's own "could this be a fully-qualified class name"
 * detection only ever reaches its own field lookup for already-loaded,
 * well-known classes (e.g. java.lang.System) - it never fired for a
 * same-compilation-batch, user-defined type, so case_expr->sem_symbol
 * stayed NULL. Fixed instead by resolving the case label directly: the
 * receiver is resolved as a type via semantic_resolve_type() (whose own
 * AST_IDENTIFIER-as-type case already resolves an imported or
 * same-package simple name through the ordinary import/classpath
 * machinery), then the field name is looked up directly on that type's
 * own members (and, if not found there, its implemented interfaces) -
 * mirroring the simpler, already-working bare-identifier case-label
 * branch instead of routing through the fragile FQN-chain logic. Once
 * resolved, the constant's own literal initializer value is read and
 * the case_expr node is transformed in place from AST_FIELD_ACCESS into
 * a plain AST_LITERAL carrying that value, exactly as the existing
 * bare-identifier case-label branch already does.
 *
 * A second gap surfaced along the way: a field symbol registered via the
 * interface/classpath-completion registration path has its own ->ast
 * pointing at the whole AST_FIELD_DECL (children: [type_node,
 * declarator, declarator, ...]), not directly at the AST_VAR_DECLARATOR
 * the initial fix assumed - so the declarator actually matching the
 * field's own name has to be found among the FIELD_DECL's children
 * rather than assumed to be the symbol's ->ast itself.
 *
 * (A same-file/same-batch qualified constant loaded from an already
 * -compiled .class file via -cp, rather than compiled together as
 * source, still isn't constant-folded here - reading a classfile's own
 * ConstantValue attribute for this purpose is a separate, deeper gap,
 * not needed for gumdrop's own single-invocation build.)
 */
public class SwitchQualifiedConstantCaseLabelVerifyTest {
    static String dispatch(int methodId) {
        switch (methodId) {
            case SwitchConstants.START:
                return "start";
            case SwitchConstants.SECURE:
                return "secure";
            case SwitchConstants.TUNE:
                return "tune";
            default:
                return "unknown";
        }
    }

    public static void main(String[] args) {
        String result = dispatch(10) + "," + dispatch(20) + "," + dispatch(30) + "," + dispatch(99);
        if (!"start,secure,tune,unknown".equals(result)) {
            throw new RuntimeException("expected start,secure,tune,unknown but got " + result);
        }
        System.out.println("SwitchQualifiedConstantCaseLabelVerifyTest passed!");
    }
}
