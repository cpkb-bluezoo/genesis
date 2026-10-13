class LambdaLocalShadowsLocal {
    interface SAM { void m(Object o); }

    void t() {
        Object local = null;
        SAM s = param -> { Object local = null; };
    }
}
