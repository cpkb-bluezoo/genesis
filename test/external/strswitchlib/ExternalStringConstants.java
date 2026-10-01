package strswitchlib;

/*
 * Deliberately compiled with REAL javac (see run-tests.sh) - mirrors
 * jakarta.servlet.http.HttpServletRequest's own DIGEST_AUTH/BASIC_AUTH
 * String constants exactly: a classfile-loaded "static final String"
 * field, whose value comes from its own ConstantValue attribute (JVMS
 * 4.7.2), not an AST initializer.
 */
public interface ExternalStringConstants {
    String DIGEST_AUTH = "DIGEST";
    String BASIC_AUTH = "BASIC";
}
