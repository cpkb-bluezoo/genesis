package genesislib;

/*
 * Compiled by genesis itself (see run-tests.sh) into its OWN directory,
 * separate compile batch from the client that references it - exactly
 * mirroring a real cross-module dependency (like gumdrop's own "core"
 * module vs its "servlet" test module), which is what actually exposed
 * this bug: a constant referenced from a DIFFERENT compile batch has no
 * AST to read an initializer from, only the classfile genesis itself
 * just wrote for it.
 */
public abstract class ScopeConstants {
    public static final int PAGE_SCOPE = 1;
    public static final int REQUEST_SCOPE = 2;
    public static final int SESSION_SCOPE = 3;
    public static final int APPLICATION_SCOPE = 4;
}
