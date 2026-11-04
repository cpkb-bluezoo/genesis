public interface LeafBuilder extends MidBuilder, Maker<LeafBuilder, Leaf> {
    LeafBuilder bucket(String name);
    LeafBuilder key(String name);
}
