package switchqualifiedconst;

/*
 * Companion interface for SwitchQualifiedConstantCaseLabelVerifyTest - see
 * that file for the bug this pair reproduces. Must be compiled together
 * with the switch-containing file in a single genesis invocation (as
 * javac/genesis do for a whole source tree, matching a real multi-file
 * project) to trigger it.
 */
public interface SwitchConstants {
    int START = 10;
    int SECURE = 20;
    int TUNE = 30;
}
