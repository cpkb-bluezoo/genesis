package interfacemembertype.caller;

import interfacemembertype.iface.Container;

/**
 * Bug: a class declared inside an interface with no explicit modifiers
 * (e.g. "interface Container { class Item { ... } }") is implicitly
 * PUBLIC and STATIC per JLS 9.5, but genesis treated it as neither:
 *
 * 1. The class's own compiled .class file (what the JVM's real
 *    access-control check at link time actually consults, not the
 *    InnerClasses attribute) was missing ACC_PUBLIC, so referencing it
 *    from a DIFFERENT PACKAGE than the declaring interface (this file)
 *    failed with "IllegalAccessError: failed to access class ...".
 * 2. Not implicitly static made codegen wrongly treat it as a
 *    non-static inner class needing an outer-this reference - adding a
 *    synthetic "this$0" field and an extra leading constructor
 *    parameter that makes no sense for a member of an INTERFACE (there
 *    is no interface "instance" to capture) - producing
 *    "NoSuchMethodError" for the constructor at any ordinary call site.
 *
 * Confirmed against gumdrop's own FtpFileSystem (an interface)
 * declaring "class DirectoryChangeResult { ... }" with no modifiers,
 * used from BasicFTPFileSystem in a different package
 * (org.bluezoo.gumdrop.ftp.file vs org.bluezoo.gumdrop.ftp).
 *
 * MUST be compiled into a freshly emptied output directory (see
 * run-tests.sh): genesis puts its own -d directory on the classpath,
 * and class files left there by an earlier compile can mask this bug.
 */
public class InterfaceMemberClassImplicitPublicStaticVerifyTest implements Container {
    @Override
    public Item makeItem(int value) {
        return new Item(value);
    }

    public static void main(String[] args) {
        Container.Item item = new Container.Item(42);
        if (item.value != 42) {
            throw new RuntimeException("expected 42, got " + item.value);
        }

        Container c = new InterfaceMemberClassImplicitPublicStaticVerifyTest();
        Container.Item viaOverride = c.makeItem(7);
        if (viaOverride.value != 7) {
            throw new RuntimeException("expected 7, got " + viaOverride.value);
        }
    }
}
