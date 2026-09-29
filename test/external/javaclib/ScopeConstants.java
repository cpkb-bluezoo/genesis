package javaclib;

/*
 * Deliberately compiled with REAL javac (see run-tests.sh), not genesis -
 * genesis's own classwriter never emits a ConstantValue attribute for a
 * "static final int" field (it uses <clinit> instead), so a genesis-
 * compiled version of this file could never exercise the bug this
 * supports testing at all: a "static final int" field of an externally-
 * compiled (real javac, real JDK/library JAR) class, inherited and
 * referenced as a bare switch case label.
 */
public abstract class ScopeConstants {
    public static final int PAGE_SCOPE = 1;
    public static final int REQUEST_SCOPE = 2;
    public static final int SESSION_SCOPE = 3;
    public static final int APPLICATION_SCOPE = 4;
}
