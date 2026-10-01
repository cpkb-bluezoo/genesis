package switchclassfileconstlib;

/*
 * Deliberately compiled by genesis itself into a SEPARATE classfile (see
 * run-tests.sh) - GitHub issue #2: a static final int field's
 * ConstantValue attribute (JVMS 4.7.2) must be read back when the
 * declaring type is loaded from an already-compiled .class file, not
 * just when it's compiled in the same batch as the code referencing it.
 */
public interface Constants {
    int START = 10;
    int SECURE = 20;
    int TUNE = 30;
}
