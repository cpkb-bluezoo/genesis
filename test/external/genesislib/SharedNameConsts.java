package genesislib;

/*
 * Deliberately declares constants sharing names with an unrelated
 * enum's own constants, brought in via wildcard static import - see
 * EnumSwitchStaticImportNameCollisionTest's own comment.
 */
public final class SharedNameConsts {
    public static final byte FOO = 0x02;
    public static final byte BAR = 0x01;
}
