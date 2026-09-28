package pkgscantest;

/*
 * Regression test: a *nested* (non-top-level) class implementing a
 * classfile-loaded interface used to fail to resolve that interface's own
 * nested type when referenced unqualified as a method return type, even
 * though the exact same pattern worked fine for a top-level implementing
 * class. The nested class's own "implements" clause wasn't resolved yet at
 * the point its method return types were checked. See
 * HandlerWithNestedType.java (compiled to a classfile first - the bug did
 * not reproduce when the interface was loaded from source) and genesis
 * history for details.
 */
class NestedClassImplementsInterfaceWithNestedType {
    private static class NoopHandler implements HandlerWithNestedType {
        public Type getType() {
            return Type.A;
        }
    }
}
