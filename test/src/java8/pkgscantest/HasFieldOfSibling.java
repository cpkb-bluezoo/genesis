package pkgscantest;

/*
 * Regression test for a crash (stack overflow / SIGSEGV) that used to occur
 * when resolving this field's type: genesis peels the package prefix off a
 * qualified candidate name to check whether it might itself be a type,
 * which sends a same-package scan through every source file in this
 * directory - including SiblingWithNestedType.java below, whose nested
 * class then triggers the same scan again before it is ever cached
 * locally, recursing without bound. See genesis history for details.
 */
class HasFieldOfSibling {
    SiblingWithNestedType sibling;
}
