/*
 * A QUALIFIED switch case label ("case Type.CONSTANT:") naming a
 * "static final int" constant declared on an INTERFACE compiled in a
 * SEPARATE genesis invocation (a different compile batch/module - see
 * genesislib/FrameSettings.java's own comment) collapsed to match value
 * 0 for every such case: resolve_qualified_constant_case_value()
 * (semantic.c) relied entirely on the field symbol's ->ast to read an
 * initializer expression from, which a classfile-loaded field (no AST,
 * only a ConstantValue attribute) never has - unlike its sibling
 * resolve_named_int_constant() (used for BARE case labels), which
 * already had a has_const_value/const_value fallback for exactly this.
 * Duplicate lookupswitch match values, VerifyError/ClassFormatError
 * "Bad lookupswitch instruction". Confirmed against gumdrop's own
 * HttpProtocolHandler.settingsFrameReceived(), whose "case
 * H2FrameHandler.SETTINGS_HEADER_TABLE_SIZE:" etc. (H2FrameHandler is an
 * interface compiled as part of a different module) are exactly this
 * shape.
 */
import genesislib.FrameSettings;

public class CrossModuleQualifiedInterfaceConstantSwitchTest {
    static String describe(int id) {
        switch (id) {
            case FrameSettings.HEADER_TABLE_SIZE:
                return "HEADER_TABLE_SIZE";
            case FrameSettings.ENABLE_PUSH:
                return "ENABLE_PUSH";
            case FrameSettings.MAX_CONCURRENT_STREAMS:
                return "MAX_CONCURRENT_STREAMS";
            case FrameSettings.INITIAL_WINDOW_SIZE:
                return "INITIAL_WINDOW_SIZE";
            default:
                return "UNKNOWN";
        }
    }

    public static void main(String[] args) {
        if (!"HEADER_TABLE_SIZE".equals(describe(1))) {
            System.out.println("FAILED: expected HEADER_TABLE_SIZE, got " + describe(1));
            System.exit(1);
        }
        if (!"ENABLE_PUSH".equals(describe(2))) {
            System.out.println("FAILED: expected ENABLE_PUSH, got " + describe(2));
            System.exit(1);
        }
        if (!"MAX_CONCURRENT_STREAMS".equals(describe(3))) {
            System.out.println("FAILED: expected MAX_CONCURRENT_STREAMS, got " + describe(3));
            System.exit(1);
        }
        if (!"INITIAL_WINDOW_SIZE".equals(describe(4))) {
            System.out.println("FAILED: expected INITIAL_WINDOW_SIZE, got " + describe(4));
            System.exit(1);
        }
        if (!"UNKNOWN".equals(describe(99))) {
            System.out.println("FAILED: expected UNKNOWN, got " + describe(99));
            System.exit(1);
        }
        System.out.println("CrossModuleQualifiedInterfaceConstantSwitchTest passed!");
    }
}
