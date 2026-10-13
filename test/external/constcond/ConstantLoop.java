class ConstantLoop { static final boolean FOREVER = true; int m() { int x; while (FOREVER) { x = 1; break; } return x; } }
