package interfacemembertype.iface;

/**
 * Declares a member class with NO explicit modifiers - per JLS 9.5, a
 * member type of an interface is implicitly public and static
 * regardless. See InterfaceMemberClassImplicitPublicStaticVerifyTest.
 *
 * The nested type is also used as an abstract method's return type
 * (mirroring gumdrop's own FtpFileSystem.changeDirectory(), whose
 * return type is its own nested DirectoryChangeResult) - a class
 * implementing this interface needs the nested type's real, resolved
 * symbol just to check its override signature, which reaches a
 * different, CROSS-FILE resolution path (this interface's own members
 * being lazily completed on demand, rather than a plain constructor
 * reference) than a bare "new Container.Item(...)" call alone does.
 */
public interface Container {
    class Item {
        public final int value;

        public Item(int value) {
            this.value = value;
        }
    }

    Item makeItem(int value);
}
