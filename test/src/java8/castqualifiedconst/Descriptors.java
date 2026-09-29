package castqualifiedconst;

/*
 * Companion interface for CastQualifiedConstantCaseLabelVerifyTest - see
 * that file for the bug this pair reproduces. Must be compiled together
 * with the switch-containing file in a single genesis invocation (as
 * javac/genesis do for a whole source tree, matching a real multi-file
 * project) to trigger it.
 */
public interface Descriptors {
    long OPEN = 0x10;
    long CLOSE = 0x18;
    long BEGIN = 0x11;
}
