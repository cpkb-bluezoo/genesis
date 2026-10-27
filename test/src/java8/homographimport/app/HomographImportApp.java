package homographimport.app;

import homographimport.lib.*;

// The imported HomographImportAPP differs only in case from this class in the same
// package, so a file lookup that ignores case on a case-insensitive file system would
// bind the name to this class's own source file instead of the import.
public final class HomographImportApp {
    private final HomographImportAPP held;

    public static HomographImportApp create() {
        final HomographImportAPP client = new CustomHomographImport();
        return new HomographImportApp(client);
    }

    private HomographImportApp(HomographImportAPP held) {
        this.held = held;
    }

    public boolean isHolding() {
        return held != null;
    }
}
