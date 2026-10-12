package pushcovlib;

public class LeafTests extends MidTests {

    @Override
    protected NarrowProps createProperties() {
        return new NarrowProps();
    }

    @Override
    protected Adapter createConfigAdapter(Props properties) {
        return new Adapter(properties);
    }
}
