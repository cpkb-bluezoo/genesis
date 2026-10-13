class SwitchLocalShadowedByLambdaLocal {
    interface SAM { void m(Object o); }

    static void t(int i) {
        switch (i) {
            case 0: Object local = null;
            default: {
                SAM s2 = param -> { Object local = null; };
            }
        }
    }
}
