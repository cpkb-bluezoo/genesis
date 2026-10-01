import switchclassfileconstlib.Constants;

/*
 * GitHub issue #2: a qualified constant ("case Type.CONSTANT:") used as a
 * switch case label silently resolved to 0 instead of the field's real
 * value whenever Type was loaded from an already-compiled .class file
 * (via -cp) rather than compiled in the same genesis invocation as the
 * switch itself - genesis had no mechanism to read the field's own
 * ConstantValue attribute (JVMS 4.7.2) from a classfile, so a case
 * label resolved via a classfile-loaded symbol (whose sym->ast is NULL -
 * there's no source AST for a classfile-loaded field) had nothing to
 * fall back to and matched nothing at runtime, with no compile error -
 * "VerifyError: Bad lookupswitch instruction" once two or more case
 * labels collapsed to the same (wrong) key 0.
 *
 * By the time this test was written, resolve_named_int_constant()/
 * resolve_qualified_constant_case_value() (semantic.c) already read
 * sym->data.var_data.has_const_value/const_value - populated from a
 * classfile field's own ConstantValue attribute when the field is
 * loaded - so this exact repro already passes. No dedicated regression
 * test exercised the TWO-STEP (separately compiled) case specifically
 * though (the existing SwitchQualifiedConstantCaseLabelVerifyTest
 * compiles both files together in one invocation, which works for a
 * different reason - the source AST's own literal initializer is still
 * available in that case), so this closes that gap.
 */
public class SwitchClassfileConstantTest {
    static String name(int code) {
        switch (code) {
            case Constants.START:  return "start";
            case Constants.SECURE: return "secure";
            case Constants.TUNE:   return "tune";
            default:                return "unknown";
        }
    }

    public static void main(String[] args) {
        String result = name(10) + "," + name(20) + "," + name(30) + "," + name(99);
        if (!"start,secure,tune,unknown".equals(result)) {
            throw new RuntimeException("expected start,secure,tune,unknown but got " + result);
        }
        System.out.println("SwitchClassfileConstantTest passed!");
    }
}
