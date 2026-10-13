class NestedBlockShadowsLocal {
    void t() {
        int local = 1;
        {
            int local = 2;
        }
    }
}
