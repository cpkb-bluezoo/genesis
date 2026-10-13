class ParameterShadowedByBlockLocal {
    void t(Object local) {
        for (int i = 0; i < 1; i++) {
            Object local = null;
        }
    }
}
