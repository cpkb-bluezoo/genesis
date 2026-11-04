public interface Store {
    Attrs read(Leaf request);
    Bytes fetch(Leaf request);
}
