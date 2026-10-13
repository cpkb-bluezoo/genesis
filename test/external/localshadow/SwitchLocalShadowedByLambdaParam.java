class SwitchLocalShadowedByLambdaParam {
    interface SAM { void m(Object o); }

    void t(int i) {
        switch (i) {
            case 0: Object local = null;
            default: {
                SAM s1 = local -> { };
            }
        }
    }
}
