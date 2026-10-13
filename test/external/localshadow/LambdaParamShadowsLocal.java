class LambdaParamShadowsLocal {
    interface SAM { void m(Object o); }

    void t() {
        Object local = null;
        SAM s = local -> { };
    }
}
