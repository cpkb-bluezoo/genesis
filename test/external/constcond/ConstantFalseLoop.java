class ConstantFalseLoop { int m() { final int limit = 0; int x; for (int i = 0; i < limit; i++) { } do { x = 2; } while (limit > 5); return x; } }
