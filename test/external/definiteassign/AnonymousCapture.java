class AnonymousCapture { Runnable m() { int x; return new Runnable() { public void run() { System.out.println(x); } }; } }
