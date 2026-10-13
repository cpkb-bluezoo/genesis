class LongConstant { static final long BIG = 1L << 40; int m() { int x; if (BIG > 1000L && BIG != 0) { x = 1; } return x; } }
