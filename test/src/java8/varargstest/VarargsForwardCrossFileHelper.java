public final class VarargsForwardCrossFileHelper extends VarargsForwardCrossFileTest {
    static VarargsForwardCrossFileHelper open(String path, String... options) {
        return new VarargsForwardCrossFileHelper(path, options.length);
    }

    final String path;
    final int optionCount;

    VarargsForwardCrossFileHelper(String path, int optionCount) {
        this.path = path;
        this.optionCount = optionCount;
    }
}
