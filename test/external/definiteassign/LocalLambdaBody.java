class LocalLambdaBody { Runnable m() { return () -> { int y; System.out.println(y); }; } }
