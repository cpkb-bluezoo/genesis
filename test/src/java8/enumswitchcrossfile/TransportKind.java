package enumswitchcrossfile;

/*
 * Companion enum for EnumOrdinalCrossFileSwitchVerifyTest - see that file
 * for the bug this pair reproduces. Must be compiled together with the
 * switch-containing file in a single genesis invocation to trigger it.
 */
public enum TransportKind {
    DOQ, DOT, DOH, PLAIN
}
