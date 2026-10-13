class AnonymousLambdaCapture { Object m(boolean c) { int x; if (c) { x = 1; } return new Object() { Runnable r = () -> System.out.println(x); }; } }
