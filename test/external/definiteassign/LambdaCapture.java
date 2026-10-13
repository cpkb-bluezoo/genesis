class LambdaCapture { Runnable m() { int x; return () -> System.out.println(x); } }
