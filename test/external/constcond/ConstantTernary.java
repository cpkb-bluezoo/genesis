class ConstantTernary { static final boolean A = true; int m() { int x; if (A ? true : false) { x = 1; } return x; } }
