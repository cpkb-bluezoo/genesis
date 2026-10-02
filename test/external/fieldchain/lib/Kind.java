package fieldchain.lib;

public enum Kind {
    SOURCE(".java");

    public final String extension;
    public final long size = 7L;

    Kind(String extension) {
        this.extension = extension;
    }
}
