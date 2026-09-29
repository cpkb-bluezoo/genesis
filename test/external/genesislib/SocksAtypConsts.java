package genesislib;

/*
 * Mirrors gumdrop's own SocksConstants - a separate class whose "byte"
 * constants are brought into a switch statement via a WILDCARD static
 * import ("import static genesislib.SocksAtypConsts.*;"), not by
 * inheritance/interfaces.
 */
public final class SocksAtypConsts {
    public static final byte ATYP_IPV4 = 0x01;
    public static final byte ATYP_DOMAINNAME = 0x03;
    public static final byte ATYP_IPV6 = 0x04;
}
