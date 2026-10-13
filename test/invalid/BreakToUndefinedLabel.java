class BreakToUndefinedLabel {
    void m() {
        inner: {
            break outer;
        }
    }
}
