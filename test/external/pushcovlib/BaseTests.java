package pushcovlib;

public abstract class BaseTests<P extends Props, A extends Adapter> {

    protected abstract P createProperties();

    protected abstract A createConfigAdapter(P properties);

    public final String stepViaBase() {
        P p = createProperties();
        return p.getClass().getSimpleName();
    }
}
